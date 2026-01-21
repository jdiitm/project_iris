#!/bin/bash
# =============================================================================
# Global Cluster Management Script
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.yml"
COMPOSE_MTLS="$SCRIPT_DIR/docker-compose.mtls.yml"

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
        echo "Waiting for cluster to stabilize..."
        sleep 10
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
        docker compose -f "$COMPOSE_FILE" --profile chaos down -v
        ;;
    
    down-mtls)
        echo "=== Stopping mTLS Cluster ==="
        docker compose -f "$COMPOSE_FILE" -f "$COMPOSE_MTLS" --profile chaos down -v
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
    
    kill-core)
        CONTAINER="${2:-core-west-1}"
        echo "=== Killing core node: $CONTAINER ==="
        docker kill "$CONTAINER"
        echo "Core killed. Observe failover behavior."
        ;;
    
    clean)
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
        echo "  kill-core [container]   Kill a core node for failover test"
        echo "  clean           Remove all containers and volumes"
        ;;
esac
