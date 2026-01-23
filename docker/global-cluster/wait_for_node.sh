#!/bin/sh
# =============================================================================
# Wait for Erlang Node Readiness
# =============================================================================
# Replaces timer:sleep() with actual readiness verification
# Usage: wait_for_node.sh <target_node> <timeout_seconds>
# =============================================================================

TARGET_NODE="$1"
TIMEOUT="${2:-30}"
COOKIE="${IRIS_COOKIE:-iris_secret}"

wait_for_node() {
    local elapsed=0
    while [ $elapsed -lt $TIMEOUT ]; do
        # Try to ping the target node
        result=$(erl -noshell -sname probe_$$ -setcookie "$COOKIE" -eval "
            case net_adm:ping('$TARGET_NODE') of
                pong -> io:format(\"ready\"), halt(0);
                pang -> halt(1)
            end." 2>/dev/null)
        
        if [ "$result" = "ready" ]; then
            return 0
        fi
        
        sleep 1
        elapsed=$((elapsed + 1))
    done
    return 1
}

wait_for_mnesia() {
    local elapsed=0
    while [ $elapsed -lt $TIMEOUT ]; do
        result=$(erl -noshell -sname probe_$$ -setcookie "$COOKIE" -eval "
            case rpc:call('$TARGET_NODE', mnesia, system_info, [is_running], 5000) of
                yes -> io:format(\"ready\"), halt(0);
                _ -> halt(1)
            end." 2>/dev/null)
        
        if [ "$result" = "ready" ]; then
            return 0
        fi
        
        sleep 1
        elapsed=$((elapsed + 1))
    done
    return 1
}

# First wait for node to be reachable
if ! wait_for_node; then
    echo "ERROR: Node $TARGET_NODE not reachable after ${TIMEOUT}s" >&2
    exit 1
fi

# Then wait for Mnesia to be running
if ! wait_for_mnesia; then
    echo "ERROR: Mnesia not running on $TARGET_NODE after ${TIMEOUT}s" >&2
    exit 1
fi

echo "Node $TARGET_NODE is ready"
exit 0
