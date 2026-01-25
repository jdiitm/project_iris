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
# Restart Any Stopped Containers
# =============================================================================

restart_stopped_containers() {
    local cores=("core-east-1" "core-east-2" "core-west-1" "core-west-2" "core-eu-1" "core-eu-2")
    
    log_info "Checking for stopped containers..."
    
    for container in "${cores[@]}"; do
        # Check if container exists but is stopped
        local status=$(docker inspect --format '{{.State.Status}}' "$container" 2>/dev/null || echo "missing")
        
        if [ "$status" = "exited" ] || [ "$status" = "created" ]; then
            log_warn "Container $container is stopped - restarting..."
            docker start "$container" 2>/dev/null || true
        fi
    done
    
    # Wait a bit for containers to start
    sleep 5
}

# =============================================================================
# Core Readiness Check
# =============================================================================

check_all_cores_ready() {
    local cores=("core-east-1" "core-east-2" "core-west-1" "core-west-2" "core-eu-1" "core-eu-2")
    local nodes=("core_east_1@coreeast1" "core_east_2@coreeast2" "core_west_1@corewest1" 
                 "core_west_2@corewest2" "core_eu_1@coreeu1" "core_eu_2@coreeu2")
    
    log_info "Checking all core nodes are ready..."
    
    local ready_count=0
    local failed_cores=""
    
    for i in "${!cores[@]}"; do
        local container="${cores[$i]}"
        local node="${nodes[$i]}"
        
        if ! docker ps --format '{{.Names}}' | grep -q "^${container}$"; then
            log_warn "Container $container not running - skipping"
            continue
        fi
        
        if wait_for_epmd "$container" 30; then
            if wait_for_mnesia "$container" "$node" 45; then
                ready_count=$((ready_count + 1))
            else
                failed_cores="$failed_cores $container"
                log_warn "Mnesia not ready on $container - continuing anyway"
            fi
        else
            failed_cores="$failed_cores $container"
            log_warn "EPMD not ready on $container - continuing anyway"
        fi
    done
    
    log_info "Ready cores: $ready_count/6 (failed:$failed_cores)"
    
    # Require at least 2 cores to be ready (one per region minimum)
    if [ $ready_count -ge 2 ]; then
        log_info "Minimum cores ready - proceeding with replication"
        return 0
    else
        log_error "Less than 2 cores ready - cannot proceed"
        return 1
    fi
}

# =============================================================================
# Mnesia Cluster Membership Verification
# =============================================================================

wait_for_mnesia_cluster() {
    local max_attempts=${1:-60}
    local expected_nodes=${2:-6}
    local attempt=0
    
    log_info "Waiting for Mnesia cluster to form (expecting $expected_nodes nodes)..."
    
    while [ $attempt -lt $max_attempts ]; do
        local db_nodes=$(docker exec core-east-1 sh -c 'erl -noshell -sname cluster_check_$RANDOM -setcookie iris_secret -eval "
            case net_adm:ping('"'"'core_east_1@coreeast1'"'"') of
                pong ->
                    DbNodes = rpc:call('"'"'core_east_1@coreeast1'"'"', mnesia, system_info, [db_nodes], 5000),
                    case DbNodes of
                        {badrpc, _} -> io:format(\"0\");
                        Nodes when is_list(Nodes) -> io:format(\"~p\", [length(Nodes)]);
                        _ -> io:format(\"0\")
                    end;
                pang -> io:format(\"0\")
            end,
            halt(0).
        "' 2>/dev/null || echo "0")
        
        if [ "$db_nodes" -ge "$expected_nodes" ] 2>/dev/null; then
            log_info "Mnesia cluster has $db_nodes nodes (expected $expected_nodes)"
            return 0
        fi
        
        if [ $((attempt % 10)) -eq 0 ]; then
            log_info "Waiting... ($attempt/$max_attempts) - current db_nodes: $db_nodes"
        fi
        
        attempt=$((attempt + 1))
        sleep 1
    done
    
    log_error "Mnesia cluster did not form with $expected_nodes nodes after ${max_attempts}s"
    return 1
}

verify_mnesia_cluster_membership() {
    log_info "Verifying Mnesia cluster membership..."
    
    docker exec core-east-1 sh -c 'erl -noshell -sname verify_cluster_$RANDOM -setcookie iris_secret -eval "
        pong = net_adm:ping('"'"'core_east_1@coreeast1'"'"'),
        
        DbNodes = rpc:call('"'"'core_east_1@coreeast1'"'"', mnesia, system_info, [db_nodes], 5000),
        RunningNodes = rpc:call('"'"'core_east_1@coreeast1'"'"', mnesia, system_info, [running_db_nodes], 5000),
        
        io:format(\"  Mnesia db_nodes: ~p~n\", [DbNodes]),
        io:format(\"  Running db_nodes: ~p~n\", [RunningNodes]),
        
        case {DbNodes, RunningNodes} of
            {{badrpc, _}, _} -> 
                io:format(\"  ERROR: Cannot query db_nodes~n\"),
                halt(1);
            {_, {badrpc, _}} -> 
                io:format(\"  ERROR: Cannot query running_db_nodes~n\"),
                halt(1);
            {D, R} when is_list(D), is_list(R) ->
                io:format(\"  Total: ~p db_nodes, ~p running~n\", [length(D), length(R)]),
                case length(R) >= 2 of
                    true -> halt(0);
                    false -> 
                        io:format(\"  WARNING: Less than 2 running nodes~n\"),
                        halt(1)
                end;
            _ ->
                io:format(\"  ERROR: Unexpected response~n\"),
                halt(1)
        end.
    "'
    
    return $?
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
    # Returns exit code 0 if all tables have >= 2 copies, 1 otherwise
    docker exec core-east-1 sh -c 'erl -noshell -sname verify_helper_$RANDOM -setcookie iris_secret -eval "
        pong = net_adm:ping('"'"'core_east_1@coreeast1'"'"'),
        Tables = [presence, offline_msg, user_status, user_meta],
        Results = lists:map(fun(T) ->
            Ram = rpc:call('"'"'core_east_1@coreeast1'"'"', mnesia, table_info, [T, ram_copies]),
            Disc = rpc:call('"'"'core_east_1@coreeast1'"'"', mnesia, table_info, [T, disc_copies]),
            case {Ram, Disc} of
                {{badrpc, _}, _} -> 
                    io:format(\"  ~p: RPC FAILED~n\", [T]),
                    false;
                {_, {badrpc, _}} -> 
                    io:format(\"  ~p: RPC FAILED~n\", [T]),
                    false;
                {RamNodes, DiscNodes} when is_list(RamNodes), is_list(DiscNodes) ->
                    Total = length(RamNodes) + length(DiscNodes),
                    io:format(\"  ~p: ~p copies (~p ram, ~p disc)~n\", 
                              [T, Total, length(RamNodes), length(DiscNodes)]),
                    Total >= 2;
                _ ->
                    io:format(\"  ~p: NOT FOUND~n\", [T]),
                    false
            end
        end, Tables),
        
        AllOk = lists:all(fun(X) -> X end, Results),
        case AllOk of
            true -> 
                io:format(\"~n  All tables have >= 2 copies - REPLICATION OK~n\"),
                halt(0);
            false ->
                io:format(\"~n  WARNING: Some tables have < 2 copies~n\"),
                halt(1)
        end.
    "'
    
    return $?
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
# Cross-Region Delivery Verification
# =============================================================================

verify_cross_region_delivery() {
    log_info "Verifying cross-region message delivery (West -> Sydney)..."
    
    # Check if required edges are running
    if ! docker ps --format '{{.Names}}' | grep -q "^edge-west-1$"; then
        log_warn "edge-west-1 not running - skipping cross-region verification"
        return 0
    fi
    if ! docker ps --format '{{.Names}}' | grep -q "^edge-sydney-1$"; then
        log_warn "edge-sydney-1 not running - skipping cross-region verification"
        return 0
    fi
    
    # Use Python script if available for more reliable socket-based test
    local verify_script="$SCRIPT_DIR/../../scripts/verify_cluster_ready.py"
    if [ -f "$verify_script" ]; then
        log_info "Running verification script..."
        if python3 "$verify_script" --quick 2>/dev/null; then
            log_info "Cross-region verification PASSED"
            return 0
        else
            log_warn "Cross-region verification failed - will retry"
            return 1
        fi
    fi
    
    # Fallback: Test via Erlang RPC that user registration replicates
    log_info "Testing user registration replication..."
    local test_user="verify_user_$(date +%s)"
    
    # Register user on West core (via edge-west-1's connected core)
    local register_result=$(docker exec edge-west-1 sh -c "erl -noshell -sname verify_reg_$RANDOM -setcookie iris_secret -eval \"
        pong = net_adm:ping('core_west_1@corewest1'),
        %% Register a test user on west core
        Result = rpc:call('core_west_1@corewest1', iris_core, register_user, 
                         [<<\\\"$test_user\\\">>, self(), #{}], 5000),
        case Result of
            ok -> io:format(\\\"registered\\\");
            {ok, _} -> io:format(\\\"registered\\\");
            _ -> io:format(\\\"failed\\\")
        end,
        halt(0).
    \"" 2>/dev/null || echo "error")
    
    if [ "$register_result" != "registered" ]; then
        log_warn "Could not register test user on West: $register_result"
        return 1
    fi
    
    # Wait for replication
    sleep 2
    
    # Check if user is visible on East core (Sydney connects to East)
    local lookup_result=$(docker exec edge-sydney-1 sh -c "erl -noshell -sname verify_lookup_$RANDOM -setcookie iris_secret -eval \"
        pong = net_adm:ping('core_east_1@coreeast1'),
        %% Lookup test user on east core
        Result = rpc:call('core_east_1@coreeast1', iris_core, lookup_user, 
                         [<<\\\"$test_user\\\">>], 5000),
        case Result of
            {ok, _} -> io:format(\\\"found\\\");
            _ -> io:format(\\\"not_found\\\")
        end,
        halt(0).
    \"" 2>/dev/null || echo "error")
    
    if [ "$lookup_result" = "found" ]; then
        log_info "Cross-region replication verified: User registered on West visible on East"
        return 0
    else
        log_warn "Cross-region replication not working: User not found on East ($lookup_result)"
        return 1
    fi
}

# =============================================================================
# Main
# =============================================================================

main() {
    log_info "=========================================="
    log_info "Cluster Initialization Starting"
    log_info "=========================================="
    
    # Step 0: Restart any stopped containers (e.g., after test kills)
    restart_stopped_containers
    
    # Count running core containers
    local running_cores=$(docker ps --format '{{.Names}}' | grep -E '^core-(east|west|eu)-[12]$' | wc -l)
    log_info "Found $running_cores running core containers"
    
    # Step 1: Wait for cores to have EPMD and Mnesia running
    # This is now more tolerant - continues if at least 2 cores are ready
    if ! check_all_cores_ready; then
        log_error "Insufficient cores ready - aborting"
        exit 1
    fi
    
    # Step 2: Wait for Mnesia cluster to form with all nodes
    # Use the count of running cores as the expected minimum
    local expected_nodes=$running_cores
    if [ "$expected_nodes" -lt 2 ]; then
        expected_nodes=2
    fi
    
    if ! wait_for_mnesia_cluster 90 "$expected_nodes"; then
        log_warn "Mnesia cluster may not have all nodes - continuing anyway"
    fi
    
    # Step 3: Verify cluster membership
    if ! verify_mnesia_cluster_membership; then
        log_warn "Cluster membership verification had warnings"
    fi
    
    # Step 4: Initialize cross-region replication (with retry)
    local max_replication_attempts=3
    local replication_success=false
    
    for attempt in $(seq 1 $max_replication_attempts); do
        log_info "Replication initialization attempt $attempt/$max_replication_attempts"
        
        if init_replication; then
            # Step 5: Reconnect edges to cores
            reconnect_edges
            
            # Wait for replication to propagate
            log_info "Waiting for replication to propagate (10s)..."
            sleep 10
            
            # Step 6: Verify replication succeeded
            if verify_replication; then
                # Step 7: Verify cross-region delivery
                if verify_cross_region_delivery; then
                    replication_success=true
                    break
                else
                    log_warn "Cross-region verification failed on attempt $attempt"
                fi
            else
                log_warn "Replication verification failed on attempt $attempt"
            fi
        else
            log_warn "Replication initialization failed on attempt $attempt"
        fi
        
        if [ $attempt -lt $max_replication_attempts ]; then
            log_info "Retrying in 5 seconds..."
            sleep 5
        fi
    done
    
    if [ "$replication_success" = true ]; then
        log_info "=========================================="
        log_info "Cluster Initialization Complete - SUCCESS"
        log_info "=========================================="
        exit 0
    else
        log_error "=========================================="
        log_error "Cluster Initialization FAILED after $max_replication_attempts attempts"
        log_error "Cross-region tests may skip"
        log_error "=========================================="
        exit 1
    fi
}

# Run main if executed directly
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    main "$@"
fi
