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
    
    # Run directly on core-east-1 using rpc:call from a helper node
    # The helper node pings core_east_1 to establish connection, then uses RPC
    # Timeout: 60 seconds for RPC (replication can take time with many nodes)
    docker exec core-east-1 sh -c 'erl -noshell -sname init_helper -setcookie iris_secret -eval "
        %% Connect to existing core node
        case net_adm:ping('"'"'core_east_1@coreeast1'"'"') of
            pong -> ok;
            pang -> 
                io:format(\"ERROR: Cannot connect to core_east_1@coreeast1~n\"),
                halt(1)
        end,
        
        %% Use rpc:call with 60 second timeout (replication adds many table copies)
        Result = rpc:call('"'"'core_east_1@coreeast1'"'"', iris_core, init_cross_region_replication, [], 60000),
        
        case Result of
            ok -> 
                io:format(\"Cross-region replication initialized successfully~n\"),
                halt(0);
            {error, Reason} ->
                io:format(\"Replication init returned error: ~p~n\", [Reason]),
                halt(1);
            {badrpc, Reason} ->
                io:format(\"RPC failed: ~p~n\", [Reason]),
                halt(1);
            Other ->
                io:format(\"Unexpected result: ~p~n\", [Other]),
                halt(1)
        end.
    "'
    
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
    
    # Use RPC to query the existing core node (not start a new node)
    # Check ram_copies + disc_copies for total replicas
    docker exec core-east-1 sh -c 'erl -noshell -sname verify_helper -setcookie iris_secret -eval "
        pong = net_adm:ping('"'"'core_east_1@coreeast1'"'"'),
        Tables = [presence, offline_msg, user_status, user_meta],
        lists:foreach(fun(T) ->
            Ram = rpc:call('"'"'core_east_1@coreeast1'"'"', mnesia, table_info, [T, ram_copies]),
            Disc = rpc:call('"'"'core_east_1@coreeast1'"'"', mnesia, table_info, [T, disc_copies]),
            case {Ram, Disc} of
                {{badrpc, _}, _} -> 
                    io:format(\"  ~p: RPC FAILED~n\", [T]);
                {_, {badrpc, _}} -> 
                    io:format(\"  ~p: RPC FAILED~n\", [T]);
                {RamNodes, DiscNodes} when is_list(RamNodes), is_list(DiscNodes) ->
                    Total = length(RamNodes) + length(DiscNodes),
                    io:format(\"  ~p: ~p copies (~p ram, ~p disc)~n\", 
                              [T, Total, length(RamNodes), length(DiscNodes)]);
                _ ->
                    io:format(\"  ~p: NOT FOUND~n\", [T])
            end
        end, Tables),
        halt(0).
    "'
}

# =============================================================================
# Reconnect Edges to Cores
# =============================================================================

reconnect_edges() {
    log_info "Reconnecting edges to cores..."
    
    # Edge to core mappings
    local edges=("edge-east-1" "edge-east-2" "edge-west-1" "edge-west-2" "edge-eu-1" "edge-eu-2" "edge-sydney-1" "edge-sydney-2" "edge-saopaulo")
    local edge_nodes=("edge_east_1@edgeeast1" "edge_east_2@edgeeast2" "edge_west_1@edgewest1" "edge_west_2@edgewest2" "edge_eu_1@edgeeu1" "edge_eu_2@edgeeu2" "edge_sydney_1@edgesydney1" "edge_sydney_2@edgesydney2" "edge_saopaulo@edgesaopaulo")
    local core_targets=("core_east_1@coreeast1" "core_east_2@coreeast2" "core_west_1@corewest1" "core_west_2@corewest2" "core_eu_1@coreeu1" "core_eu_2@coreeu2" "core_east_1@coreeast1" "core_east_2@coreeast2" "core_west_1@corewest1")
    
    for i in "${!edges[@]}"; do
        local edge="${edges[$i]}"
        local edge_node="${edge_nodes[$i]}"
        local core_node="${core_targets[$i]}"
        
        if docker ps --format '{{.Names}}' | grep -q "^${edge}$"; then
            # Ping core from edge to establish connection
            docker exec "$edge" sh -c "erl -noshell -sname reconn_$RANDOM -setcookie iris_secret -eval \"
                net_adm:ping('$core_node'),
                halt(0).
            \"" 2>/dev/null || true
        fi
    done
    
    log_info "Edge reconnection complete"
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
    
    # Step 3: Reconnect edges to cores
    reconnect_edges
    
    # Step 4: Verify
    verify_replication
    
    log_info "=========================================="
    log_info "Cluster Initialization Complete"
    log_info "=========================================="
}

# Run main if executed directly
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    main "$@"
fi
