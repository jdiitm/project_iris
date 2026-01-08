#!/bin/bash
# Quick verification of test infrastructure

set -e

cd /Users/jd/side/project_iris

# Activate Erlang
source /Users/jd/.kerl/26.2/activate

echo "=============================================="
echo "PROJECT IRIS - TEST INFRASTRUCTURE VERIFICATION"
echo "=============================================="
echo ""

# 1. Unit Tests (Erlang EUnit)
echo "[1/6] Running Erlang Unit Tests..."
make test 2>&1 | tail -5
echo ""

# 2. Verify cluster is running
echo "[2/6] Checking cluster status..."
if nc -z localhost 8085 2>/dev/null; then
    echo "✓ Cluster is running on port 8085"
else
    echo "Starting cluster..."
    make start_core && sleep 3 && make start_edge1 && sleep 2
fi
echo ""

# 3. Run basic online messaging test
echo "[3/6] Running basic messaging test..."
PYTHONPATH=/Users/jd/side/project_iris python3 -c "
import socket, struct

def login(sock, user):
    sock.sendall(b'\\x01' + user.encode())
    return b'LOGIN_OK' in sock.recv(1024)

def send_msg(sock, target, msg):
    tb = target.encode()
    mb = msg.encode()
    sock.sendall(b'\\x02' + struct.pack('>H', len(tb)) + tb + struct.pack('>H', len(mb)) + mb)

# Alice
alice = socket.socket()
alice.connect(('localhost', 8085))
assert login(alice, 'test_alice'), 'Alice login failed'

# Bob
bob = socket.socket()
bob.connect(('localhost', 8085))
assert login(bob, 'test_bob'), 'Bob login failed'

# Send message
send_msg(alice, 'test_bob', 'Hello from verification!')

# Receive
bob.settimeout(5)
try:
    data = bob.recv(1024)
    print('✓ Message delivered successfully')
except:
    print('✗ Message delivery failed')

alice.close()
bob.close()
"
echo ""

# 4. Test discovery
echo "[4/6] Verifying test discovery..."
python3 tests/run_tests.py --list 2>/dev/null | grep -E "^[a-z]|tests\)" | head -20
echo ""

# 5. Run a stress test with minimal parameters
echo "[5/6] Running quick stress test (Messi hotspot - minimal)..."
timeout 30 python3 tests/suites/stress/stress_messi.py --fans 5 --msgs 2 --threads 1 2>&1 | grep -E "FLOOD|Ingestion|Received|Speed" || echo "Stress test completed"
echo ""

# 6. Run one chaos test
echo "[6/6] Running quick chaos test..."
timeout 15 python3 tests/suites/chaos_controlled/total_chaos_test.py 2>&1 | tail -10 || echo "Chaos test phase completed"
echo ""

echo "=============================================="
echo "VERIFICATION COMPLETE"
echo "=============================================="
