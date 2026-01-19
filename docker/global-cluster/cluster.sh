#!/bin/bash
# =============================================================================
# Global Cluster Management Script
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.yml"

case "${1:-help}" in
    up)
        echo "=== Starting Global Cluster ==="
        docker compose -f "$COMPOSE_FILE" up -d
        echo "Waiting for cluster to stabilize..."
        sleep 10
        docker compose -f "$COMPOSE_FILE" ps
        ;;
    
    up-chaos)
        echo "=== Starting Global Cluster with Chaos Injection ==="
        docker compose -f "$COMPOSE_FILE" --profile chaos up -d
        echo "Waiting for cluster to stabilize..."
        sleep 10
        docker compose -f "$COMPOSE_FILE" ps
        ;;
    
    down)
        echo "=== Stopping Global Cluster ==="
        docker compose -f "$COMPOSE_FILE" --profile chaos down -v
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
        echo "  up              Start cluster (no chaos)"
        echo "  up-chaos        Start cluster with latency injection"
        echo "  down            Stop cluster"
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
