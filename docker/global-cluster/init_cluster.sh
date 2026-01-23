#!/bin/bash
# =============================================================================
# Cluster Initialization Script
# =============================================================================
# This script waits for all core nodes to be ready and then initializes
# cross-region Mnesia replication. It replaces timing-based sleeps with
# explicit readiness checks.
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COOKIE="${IRIS_COOKIE:-iris_secret}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# =============================================================================
# Readiness Checks (Replaces timer:sleep)
# =============================================================================

wait_for_epmd() {
    local host=$1
    local max_attempts=${2:-30}
    local attempt=0
    
    log_info "Waiting for EPMD on $host..."
    while [ $attempt -lt $max_attempts ]; do
        if docker exec "$host" epmd -names 2>/dev/null | grep -q "name"; then
            log_info "EPMD ready on $host"
            return 0
        fi
        attempt=$((attempt + 1))
        sleep 1
    done
    log_error "EPMD not ready on $host after ${max_attempts}s"
    return 1
}

wait_for_mnesia() {
    local container=$1
    local node_name=$2
    local max_attempts=${3:-60}
    local attempt=0
    
    log_info "Waiting for Mnesia on $node_name..."
    while [ $attempt -lt $max_attempts ]; do
        local status=$(docker exec "$container" erl -noshell -sname probe_$RANDOM \
            -setcookie "$COOKIE" -eval "
            case rpc:call('$node_name', mnesia, system_info, [is_running], 5000) of
                yes -> io:format(\"ready\"), halt(0);
                _ -> halt(1)
            end." 2>/dev/null || echo "not_ready")
        
        if [ "$status" = "ready" ]; then
            log_info "Mnesia ready on $node_name"
            return 0
        fi
        attempt=$((attempt + 1))
        sleep 1
    done
    log_error "Mnesia not ready on $node_name after ${max_attempts}s"
    return 1
}

wait_for_node_connected() {
    local from_container=$1
    local from_node=$2
    local target_node=$3
    local max_attempts=${4:-30}
    local attempt=0
    
    log_info "Waiting for $from_node to connect to $target_node..."
    while [ $attempt -lt $max_attempts ]; do
        local result=$(docker exec "$from_container" erl -noshell -sname probe_$RANDOM \
            -setcookie "$COOKIE" -eval "
            case net_adm:ping('$target_node') of
                pong -> io:format(\"connected\"), halt(0);
                pang -> halt(1)
            end." 2>/dev/null || echo "not_connected")
        
        if [ "$result" = "connected" ]; then
            log_info "$from_node connected to $target_node"
            return 0
        fi
        attempt=$((attempt + 1))
        sleep 1
    done
    log_error "$from_node could not connect to $target_node after ${max_attempts}s"
    return 1
}

# =============================================================================
# Core Readiness Check
# =============================================================================

check_all_cores_ready() {
    local cores=("core-east-1" "core-east-2" "core-west-1" "core-west-2" "core-eu-1" "core-eu-2")
    local nodes=("core_east_1@coreeast1" "core_east_2@coreeast2" "core_west_1@corewest1" 
                 "core_west_2@corewest2" "core_eu_1@coreeu1" "core_eu_2@coreeu2")
    
    log_info "Checking all core nodes are ready..."
    
    for i in "${!cores[@]}"; do
        local container="${cores[$i]}"
        local node="${nodes[$i]}"
        
        if ! docker ps --format '{{.Names}}' | grep -q "^${container}$"; then
            log_warn "Container $container not running - skipping"
            continue
        fi
        
        wait_for_epmd "$container" 30 || return 1
        wait_for_mnesia "$container" "$node" 60 || return 1
    done
    
    log_info "All running core nodes have Mnesia ready"
    return 0
}

# =============================================================================
# Initialize Cross-Region Replication
# =============================================================================

init_replication() {
    log_info "Initializing cross-region Mnesia replication..."
    
    # Run from core-east-1 (primary)
    docker exec core-east-1 erl -noshell -sname init_repl_$RANDOM \
        -setcookie "$COOKIE" -pa /app/ebin -eval "
        %% First ensure we're connected to all cores
        Cores = [
            'core_east_2@coreeast2',
            'core_west_1@corewest1', 
            'core_west_2@corewest2',
            'core_eu_1@coreeu1',
            'core_eu_2@coreeu2'
        ],
        
        %% Ping all cores to establish connections
        lists:foreach(fun(Core) ->
            case net_adm:ping(Core) of
                pong -> io:format(\"Connected to ~p~n\", [Core]);
                pang -> io:format(\"WARNING: Could not connect to ~p~n\", [Core])
            end
        end, Cores),
        
        %% Wait for Mnesia to sync
        timer:sleep(2000),
        
        %% Now initialize replication
        case catch iris_core:init_cross_region_replication() of
            ok -> 
                io:format(\"Cross-region replication initialized successfully~n\"),
                halt(0);
            {error, Reason} ->
                io:format(\"Replication init failed: ~p~n\", [Reason]),
                halt(1);
            {'EXIT', Error} ->
                io:format(\"Replication init crashed: ~p~n\", [Error]),
                halt(1)
        end.
    "
    
    local result=$?
    if [ $result -eq 0 ]; then
        log_info "Cross-region replication initialized successfully"
    else
        log_error "Failed to initialize cross-region replication"
        return 1
    fi
}

# =============================================================================
# Verify Replication
# =============================================================================

verify_replication() {
    log_info "Verifying Mnesia replication status..."
    
    docker exec core-east-1 erl -noshell -sname verify_$RANDOM \
        -setcookie "$COOKIE" -eval "
        Tables = [presence, offline_msg, user_status, user_meta],
        lists:foreach(fun(T) ->
            case catch mnesia:table_info(T, where_to_read) of
                {'EXIT', _} -> 
                    io:format(\"  ~p: NOT FOUND~n\", [T]);
                Nodes ->
                    io:format(\"  ~p: ~p nodes~n\", [T, length(Nodes)])
            end
        end, Tables),
        halt(0).
    "
}

# =============================================================================
# Main
# =============================================================================

main() {
    log_info "=========================================="
    log_info "Cluster Initialization Starting"
    log_info "=========================================="
    
    # Step 1: Wait for all cores
    if ! check_all_cores_ready; then
        log_error "Not all cores are ready - aborting"
        exit 1
    fi
    
    # Step 2: Initialize replication
    if ! init_replication; then
        log_error "Replication initialization failed - aborting"
        exit 1
    fi
    
    # Step 3: Verify
    verify_replication
    
    log_info "=========================================="
    log_info "Cluster Initialization Complete"
    log_info "=========================================="
}

# Run main if executed directly
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    main "$@"
fi
