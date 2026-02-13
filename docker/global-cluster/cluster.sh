#!/bin/bash
# =============================================================================
# Global Cluster Management Script
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.yml"
COMPOSE_MTLS="$SCRIPT_DIR/docker-compose.mtls.yml"

# HS-1 AUDIT FIX: Safety checks for destructive operations
check_production_safety() {
    local operation="$1"
    
    # Check IRIS_ENV
    if [ "$IRIS_ENV" = "prod" ] || [ "$IRIS_ENV" = "production" ]; then
        echo ""
        echo "============================================================"
        echo "ERROR: DESTRUCTIVE OPERATION BLOCKED IN PRODUCTION"
        echo "============================================================"
        echo ""
        echo "IRIS_ENV is set to '$IRIS_ENV'"
        echo "Operation '$operation' would destroy data."
        echo ""
        echo "If you REALLY need to do this in production:"
        echo "  1. Unset IRIS_ENV: unset IRIS_ENV"
        echo "  2. Use --force flag: $0 $operation --force"
        echo ""
        echo "============================================================"
        exit 1
    fi
}

confirm_destructive_operation() {
    local operation="$1"
    local force_flag="$2"
    
    # Check production safety first
    check_production_safety "$operation"
    
    # If --force flag provided, skip confirmation
    if [ "$force_flag" = "--force" ]; then
        echo "[WARN] --force flag provided, skipping confirmation"
        return 0
    fi
    
    # Interactive confirmation
    echo ""
    echo "============================================================"
    echo "WARNING: DESTRUCTIVE OPERATION"
    echo "============================================================"
    echo ""
    echo "This operation will DELETE Mnesia data from secondary cores."
    echo "All messages and state on those nodes will be PERMANENTLY LOST."
    echo ""
    echo "Type 'DELETE' to confirm, or Ctrl+C to cancel:"
    echo ""
    read -r confirmation
    
    if [ "$confirmation" != "DELETE" ]; then
        echo ""
        echo "Operation cancelled (expected 'DELETE', got '$confirmation')"
        exit 1
    fi
    
    echo ""
    echo "[OK] Confirmation received, proceeding..."
}

# Function to generate certificates if missing
ensure_certs() {
    if [ ! -f "$PROJECT_ROOT/certs/ca.pem" ]; then
        echo "=== Generating mTLS Certificates ==="
        cd "$PROJECT_ROOT/certs"
        bash generate_certs.sh
        cd "$SCRIPT_DIR"
    fi
}

# Function to create node-specific certificate symlinks
setup_node_certs() {
    local NODE=$1
    local CERT_DIR="$PROJECT_ROOT/certs"
    
    # Create symlink for generic 'node.pem' and 'node.key'
    # Each container mounts certs/ and uses node.pem/node.key
    # But since we have node-specific certs, we handle this in compose
}

case "${1:-help}" in
    up)
        echo "=== Starting Global Cluster ==="
        docker compose -f "$COMPOSE_FILE" up -d
        echo ""
        echo "=== Initializing Cluster (with readiness checks) ==="
        # Use explicit readiness checks instead of sleep
        "$SCRIPT_DIR/init_cluster.sh"
        echo ""
        docker compose -f "$COMPOSE_FILE" ps
        ;;
    
    up-mtls)
        echo "=== Starting Global Cluster with mTLS ==="
        ensure_certs
        docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" up -d
        echo "Waiting for cluster to stabilize..."
        sleep 15
        docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" ps
        echo ""
        echo "mTLS enabled. Verify with: $0 verify-mtls"
        ;;
    
    up-chaos)
        echo "=== Starting Global Cluster with Chaos Injection ==="
        docker compose -f "$COMPOSE_FILE" --profile chaos up -d
        echo "Waiting for cluster to stabilize..."
        sleep 10
        docker compose -f "$COMPOSE_FILE" ps
        ;;
    
    up-chaos-mtls)
        echo "=== Starting Global Cluster with Chaos + mTLS ==="
        ensure_certs
        docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" --profile chaos up -d
        echo "Waiting for cluster to stabilize..."
        sleep 15
        docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" ps
        ;;
    
    down)
        echo "=== Stopping Global Cluster ==="
        docker compose -f "$COMPOSE_FILE" --profile chaos down -v --remove-orphans 2>/dev/null || true
        # Force-remove any dead/stale containers left by SIGKILL during chaos tests
        for c in core-east-1 core-east-2 core-west-1 core-west-2 core-eu-1 core-eu-2 \
                 edge-east-1 edge-east-2 edge-west-1 edge-west-2 edge-eu-1 edge-eu-2 \
                 edge-sydney-1 edge-sydney-2 edge-saopaulo loadgen-east loadgen-west \
                 pumba-sydney pumba-saopaulo pumba-partition-west pumba-partition-eu pumba-backbone-flap; do
            docker rm -f "$c" 2>/dev/null || true
        done
        # Clean up any leftover networks
        docker network ls --filter name=global-cluster -q | xargs -r docker network rm 2>/dev/null || true
        ;;
    
    down-mtls)
        echo "=== Stopping mTLS Cluster ==="
        docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" --profile chaos down -v --remove-orphans 2>/dev/null || true
        # Force-remove any dead/stale containers left by SIGKILL during chaos tests
        for c in core-east-1 core-east-2 core-west-1 core-west-2 core-eu-1 core-eu-2 \
                 edge-east-1 edge-east-2 edge-west-1 edge-west-2 edge-eu-1 edge-eu-2 \
                 edge-sydney-1 edge-sydney-2 edge-saopaulo loadgen-east loadgen-west \
                 pumba-sydney pumba-saopaulo pumba-partition-west pumba-partition-eu pumba-backbone-flap; do
            docker rm -f "$c" 2>/dev/null || true
        done
        # Clean up any leftover networks
        docker network ls --filter name=global-cluster -q | xargs -r docker network rm 2>/dev/null || true
        ;;
    
    verify-mtls)
        echo "=== Verifying mTLS Configuration ==="
        echo ""
        echo "1. Checking certificate presence..."
        if docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" exec core-east-1 \
            ls /app/certs/ca.pem /app/certs/core-east-1.pem 2>/dev/null; then
            echo "   ✓ Certificates mounted"
        else
            echo "   ✗ Certificates missing!"
            exit 1
        fi
        
        echo ""
        echo "2. Checking SSL application..."
        if docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" exec core-east-1 \
            erl -noshell -eval 'io:format("~p~n", [application:which_applications()]), init:stop().' 2>/dev/null | grep -q ssl; then
            echo "   ✓ SSL application loaded"
        else
            echo "   ⚠ SSL may not be loaded (check logs)"
        fi
        
        echo ""
        echo "3. Checking node connectivity..."
        docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" exec core-east-1 \
            erl -noshell -sname verify_tmp -setcookie iris_secret \
            -eval 'io:format("Connected nodes: ~p~n", [nodes()]), init:stop().' 2>/dev/null || true
        
        echo ""
        echo "=== mTLS Verification Complete ==="
        ;;
    
    status)
        echo "=== Cluster Status ==="
        docker compose -f "$COMPOSE_FILE" ps
        echo ""
        echo "=== Network Status ==="
        docker network ls | grep iris || true
        ;;
    
    logs)
        CONTAINER="${2:-core-east-1}"
        echo "=== Logs for $CONTAINER ==="
        docker compose -f "$COMPOSE_FILE" logs -f "$CONTAINER"
        ;;
    
    shell)
        CONTAINER="${2:-core-east-1}"
        echo "=== Shell on $CONTAINER ==="
        docker compose -f "$COMPOSE_FILE" exec "$CONTAINER" sh
        ;;
    
    erl)
        CONTAINER="${2:-core-east-1}"
        NODE="${3:-core_east_1@core-east-1}"
        echo "=== Erlang Remote Shell on $NODE via $CONTAINER ==="
        docker compose -f "$COMPOSE_FILE" exec "$CONTAINER" \
            erl -name debug@localhost -setcookie iris_secret -hidden \
                -remsh "$NODE"
        ;;
    
    partition)
        CONTAINER="${2:-edge-sydney-1}"
        echo "=== Simulating partition: Disconnecting $CONTAINER from backbone ==="
        docker network disconnect iris_backbone "$CONTAINER" || true
        echo "Partition active. Reconnect with: $0 reconnect $CONTAINER"
        ;;
    
    reconnect)
        CONTAINER="${2:-edge-sydney-1}"
        echo "=== Reconnecting $CONTAINER to backbone ==="
        docker network connect global-cluster_iris_backbone "$CONTAINER" || true
        ;;
    
    setup-replication)
        # HS-1 AUDIT FIX: Add safety check for destructive operation
        FORCE_FLAG="${2:-}"
        confirm_destructive_operation "setup-replication" "$FORCE_FLAG"
        
        echo "=== Setting up Cross-Region Mnesia Replication ==="
        echo "This will repair/rebuild the Mnesia cluster and replicate tables."
        echo ""
        
        # First, form a proper Mnesia cluster by having secondaries join the primary
        echo "Step 1: Rebuilding Mnesia cluster..."
        echo "This requires stopping secondary cores and clearing their Mnesia data."
        echo ""
        
        SECONDARY_CORES="core-east-2 core-west-1 core-west-2 core-eu-1 core-eu-2"
        
        for core in $SECONDARY_CORES; do
            echo "  Stopping $core..."
            docker stop "$core" 2>/dev/null || true
        done
        
        echo ""
        echo "  Clearing Mnesia data on secondaries..."
        for core in $SECONDARY_CORES; do
            # HS-1 FIX: Only delete if container is running/exists
            if docker inspect "$core" > /dev/null 2>&1; then
                docker exec "$core" rm -rf /data/mnesia/* 2>/dev/null || true
            fi
        done
        
        echo ""
        echo "  Starting secondary cores (they will join primary)..."
        for core in $SECONDARY_CORES; do
            docker start "$core" 2>/dev/null || true
            sleep 3
        done
        
        echo ""
        echo "  Waiting for cluster to stabilize..."
        sleep 15
        
        # Now run replication setup
        echo ""
        echo "Step 2: Configuring table replication..."
        docker exec core-east-1 erl -noshell -sname replication_setup -setcookie iris_secret -pa /app/ebin -eval '
            MainNode = core_east_1@coreeast1,
            pong = net_adm:ping(MainNode),
            timer:sleep(500),
            
            %% Hot reload iris_core on main node
            rpc:call(MainNode, code, purge, [iris_core]),
            rpc:call(MainNode, code, load_file, [iris_core]),
            
            %% Connect all nodes
            OtherNodes = [core_east_2@coreeast2, core_west_1@corewest1, core_west_2@corewest2, core_eu_1@coreeu1, core_eu_2@coreeu2],
            lists:foreach(fun(N) -> net_adm:ping(N) end, OtherNodes),
            timer:sleep(2000),
            
            io:format("Connected nodes: ~p~n", [nodes()]),
            
            %% Call replication setup
            Result = rpc:call(MainNode, iris_core, init_cross_region_replication, []),
            io:format("Replication result: ~p~n", [Result]),
            init:stop().
        ' 2>&1
        
        echo ""
        echo "Step 3: Verifying replication..."
        docker exec core-east-1 erl -noshell -sname verify_replication -setcookie iris_secret -pa /app/ebin -eval '
            MainNode = core_east_1@coreeast1,
            pong = net_adm:ping(MainNode),
            
            DbNodes = rpc:call(MainNode, mnesia, system_info, [db_nodes]),
            RunningNodes = rpc:call(MainNode, mnesia, system_info, [running_db_nodes]),
            io:format("Mnesia db_nodes: ~p~n", [DbNodes]),
            io:format("Mnesia running_db_nodes: ~p~n", [RunningNodes]),
            
            io:format("~nTable Distribution:~n"),
            Tables = [presence, offline_msg, user_status, user_meta],
            lists:foreach(fun(T) ->
                Ram = rpc:call(MainNode, mnesia, table_info, [T, ram_copies]),
                Disc = rpc:call(MainNode, mnesia, table_info, [T, disc_copies]),
                DiscOnly = rpc:call(MainNode, mnesia, table_info, [T, disc_only_copies]),
                io:format("  ~p: ram=~p disc=~p disc_only=~p~n", [T, Ram, Disc, DiscOnly])
            end, Tables),
            init:stop().
        ' 2>&1
        
        echo ""
        echo "=== Cross-Region Replication Setup Complete ==="
        ;;
    
    kill-core)
        CONTAINER="${2:-core-west-1}"
        echo "=== Killing core node: $CONTAINER ==="
        docker kill "$CONTAINER"
        echo "Core killed. Observe failover behavior."
        ;;
    
    clean)
        # HS-1 AUDIT FIX: Add safety check for destructive operation
        FORCE_FLAG="${2:-}"
        confirm_destructive_operation "clean" "$FORCE_FLAG"
        
        echo "=== Cleaning up all Iris containers and volumes ==="
        docker compose -f "$COMPOSE_FILE" --profile chaos down -v --remove-orphans
        docker volume ls | grep mnesia | awk '{print $2}' | xargs -r docker volume rm
        ;;
    
    help|*)
        echo "Global Cluster Management"
        echo ""
        echo "Usage: $0 <command> [args]"
        echo ""
        echo "Commands:"
        echo "  up              Start cluster (no chaos, no mTLS)"
        echo "  up-mtls         Start cluster with mTLS enforced"
        echo "  up-chaos        Start cluster with latency injection"
        echo "  up-chaos-mtls   Start cluster with chaos + mTLS"
        echo "  down            Stop cluster"
        echo "  down-mtls       Stop mTLS cluster"
        echo "  verify-mtls     Verify mTLS configuration"
        echo "  status          Show cluster status"
        echo "  logs [container]  Show logs (default: core-east-1)"
        echo "  shell [container] Open shell in container"
        echo "  erl [container] [node]  Connect Erlang remote shell"
        echo "  partition [container]   Disconnect container from backbone"
        echo "  reconnect [container]   Reconnect container to backbone"
        echo "  setup-replication [--force]  Enable cross-region Mnesia replication (DESTRUCTIVE)"
        echo "  kill-core [container]   Kill a core node for failover test"
        echo "  clean [--force]         Remove all containers and volumes (DESTRUCTIVE)"
        echo ""
        echo "Safety:"
        echo "  Destructive commands require confirmation or --force flag."
        echo "  Commands are blocked when IRIS_ENV=prod or IRIS_ENV=production."
        ;;
esac
