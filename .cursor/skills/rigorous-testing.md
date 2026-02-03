# Rigorous Testing Skill for Project Iris

## Skill Name: `rigorous-testing`

## Purpose
Zero-tolerance testing methodology with TLS enforcement, comprehensive log analysis, real-time failure detection, and immediate fix iteration. No errors swallowed, no tests skipped without explicit RFC-compliant reason, all failures analyzed line-by-line.

---

## Key Principles (MANDATORY)

1. **TLS is ENFORCED** - All tests use `config/test_tls.config`
2. **No errors swallowed** - Every error pattern in logs must be addressed
3. **No silent skips** - Every skip must have SKIP:REASON documented
4. **Line-by-line analysis** - Read every line of every log
5. **Iterate until clean** - Test → Fix → Test until 100% pass with 0 errors

---

## Quick Reference Commands

```bash
# Navigate to project
cd /home/j/.gemini/antigravity/scratch/project_iris

# STEP 1: ALWAYS nuke cluster first (use sudo if needed)
sudo pkill -9 -f beam.smp; sudo pkill -9 epmd; \
sudo docker kill $(docker ps -q) 2>/dev/null; \
sudo docker compose -f docker/global-cluster/docker-compose.yml down --remove-orphans --volumes 2>/dev/null; \
for p in 4369 8085 8086 8087 8088 8089 8090 8091 8092 8093 8094; do sudo fuser -k $p/tcp 2>/dev/null; done; \
sudo rm -rf /tmp/mnesia_* /tmp/Mnesia.* Mnesia.*

# STEP 2: Start TLS-enabled server
erl -pa ebin -noshell -sname iris_test -setcookie iris_secret \
    -config config/test_tls \
    -eval "application:ensure_all_started(iris_core), application:ensure_all_started(iris_edge)." &

# Wait for server
sleep 5 && ss -tlnp | grep 8085

# STEP 3: Run tests with log persistence
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_DIR="tests/artifacts/runs/${TIMESTAMP}"
mkdir -p "${LOG_DIR}"
python3 tests/run_tests.py --all 2>&1 | tee "${LOG_DIR}/full_test.log"

# STEP 4: Analyze logs
python3 tests/run_tests.py --analyze-log "${LOG_DIR}/full_test.log"
```

---

## Test Suites (115+ tests total)

| Suite | Tests | Duration | Requires | TLS |
|-------|-------|----------|----------|-----|
| `unit` | 2 files | ~10s | Local compile | N/A |
| `integration` | 22 | ~2 min | TLS server | ✅ |
| `e2e` | 5 | ~1 min | TLS server | ✅ |
| `security` | 7 | ~1 min | TLS server | ✅ |
| `resilience` | 3 | ~1 min | TLS server | ✅ |
| `stress` | 9 | ~8 min | TLS server | ✅ |
| `chaos_dist` | 12 | ~22 min | Docker cluster | ✅ |
| `compatibility` | 6 sub-tests | ~15s | TLS server | ✅ |
| `contract` | 1 | ~15s | TLS server | ✅ |
| `performance_light` | 1 | ~1 min | TLS server | ✅ |

**Total: 115+ tests, ~60 minutes with Docker**

---

## TLS Configuration (NFR-14 Compliant)

### Server Configuration
```erlang
%% config/test_tls.config
[
    {iris_edge, [
        {port, 8085},
        {tls, [
            {certfile, "certs/edge-east-1.pem"},
            {keyfile, "certs/edge-east-1.key"},
            {cacertfile, "certs/ca.pem"}
        ]}
    ]}
].
```

### Python Client (Default TLS)
```python
# tests/utilities/iris_client.py - TLS enabled by default
from tests.utilities.iris_client import IrisClient
client = IrisClient(host='localhost', port=8085)  # Uses TLS
client.connect()
client.login('test_user')
```

### Chaos_dist TLS Helpers
```python
# tests/suites/chaos_dist/utils.py
from tests.suites.chaos_dist.utils import create_tls_socket, tls_connect_and_login
sock = create_tls_socket('localhost', 8085)
sock = tls_connect_and_login('localhost', 8085, 'user123')
```

---

## Error Patterns to ALWAYS Flag

### Critical Errors (Must Fix)
```
FAIL
ERROR
Exception
Traceback
crash
timeout / TIMEOUT
badrpc / noproc / badarg
Connection refused
BrokenPipe
SSLError / ssl.SSLError
ConnectionResetError
0.0% delivery
0/X messages
Message NOT found
```

### Warnings (Must Investigate)
```
SKIP
⚠ / WARN / Warning
deprecated
not found / missing
timed out
```

### Success Validation
```
✓ PASS / PASS: / [PASS]
ALL TESTS PASSED
100% delivery
```

---

## Failure Categories & Fixes (Lessons Learned)

### 1. TLS Handshake Failures
```
Pattern: "Connection refused" / "ssl.SSLError" / "BrokenPipeError"

Root Cause: Client using plaintext TCP to TLS-enabled server

Fix:
- Ensure client uses ssl.SSLContext
- Load CA certificate: context.load_verify_locations('certs/ca.pem')
- Wrap socket: context.wrap_socket(sock, server_hostname='localhost')
```

### 2. Reliable Message Protocol (0% Delivery)
```
Pattern: "0.0% delivery rate" / "Msg timed out (No ACK)"

Root Cause: Client not sending ACKs for received messages (opcode 0x10)

Fix:
- Parse incoming messages for opcode 0x10 (reliable message)
- Extract message ID from payload
- Send ACK (opcode 0x03 + message_id)
- See test_bridge_durability.py _parse_and_ack_messages()
```

### 3. Non-Blocking Socket Issues
```
Pattern: "The operation did not complete (read)" / "SSLWantReadError"

Root Cause: setblocking(False) doesn't work well with TLS

Fix:
- Use settimeout(0.1) instead of setblocking(False)
- Handle ssl.SSLWantReadError in except block
- See test_bridge_durability.py _listen_loop()
```

### 4. Docker Cluster Not Ready
```
Pattern: "Connection refused" on port 8085-8094

Root Cause: Containers not fully started or Mnesia not initialized

Fix:
- Wait for containers to be healthy (30s minimum)
- Run init_cluster.sh after docker-compose up
- Verify with: docker exec core-east-1 epmd -names
```

### 5. Port Already In Use
```
Pattern: "Address already in use" / "Port 8085 still in use"

Root Cause: Previous test run left processes running

Fix:
- Use comprehensive nuke command (see below)
- Run: fuser -k 8085/tcp
- Verify: ss -tlnp | grep 8085
```

---

## Comprehensive Nuke Command

**ALWAYS run before test execution:**

```bash
sudo pkill -9 -f beam.smp; \
sudo pkill -9 epmd; \
sudo docker kill $(docker ps -q) 2>/dev/null; \
sudo docker compose -f docker/global-cluster/docker-compose.yml down --remove-orphans --volumes 2>/dev/null; \
for p in 4369 8085 8086 8087 8088 8089 8090 8091 8092 8093 8094; do sudo fuser -k $p/tcp 2>/dev/null; done; \
sudo rm -rf /tmp/mnesia_* /tmp/Mnesia.* Mnesia.*
```

Or use the test runner:
```bash
python3 tests/run_tests.py --nuke
```

---

## Iteration Protocol

### Standard Test-Fix-Test Loop

```
1. NUKE cluster (clean slate)
2. START TLS-enabled server
3. RUN single test file
4. PERSIST logs to artifacts/
5. READ logs line-by-line
6. IDENTIFY errors/warnings/skips
7. FIX issues in code or test
8. REPEAT from step 1 until clean
9. MOVE to next test file
10. After all tests pass, run FULL suite
```

### Strict Mode Execution
```bash
# Fail on ANY warning or skip
python3 tests/run_tests.py --all --strict

# Analyze specific log
python3 tests/run_tests.py --analyze-log tests/artifacts/runs/TIMESTAMP/full_test.log
```

---

## Log Analysis Protocol

### After Every Test Run
```bash
# 1. Count results
grep -c '✓ PASS:' LOG_FILE
grep -c '✗ FAIL:' LOG_FILE  
grep -c '⚠ SKIP:' LOG_FILE

# 2. Extract all errors with context
grep -B5 -A10 -iE "FAIL|ERROR|Exception" LOG_FILE

# 3. Check for swallowed errors (errors that didn't cause failure)
grep -iE "error|exception|crash|badrpc" LOG_FILE | grep -v "✗ FAIL"

# 4. Verify no 0-message scenarios
grep -iE "0 messages|0.0%|0/[0-9]+" LOG_FILE
```

### Required Report Format
```markdown
# Test Run Report: TIMESTAMP

## Summary
| Metric | Count |
|--------|-------|
| Total  | 115+  |
| Passed | X     |
| Failed | Y     |
| Skipped| Z     |

## Failed Tests (if any)
| Test | Error | Root Cause | Fix Applied |
|------|-------|------------|-------------|
| ... | ... | ... | ... |

## Warnings Investigated
- [line X] warning message → verified harmless / fixed

## Swallowed Errors (MUST be zero)
- None (or list each with explanation)

## Verification
- [ ] All 115+ tests executed
- [ ] Zero failures
- [ ] Zero unexplained skips
- [ ] Log analysis clean
```

---

## Prohibited Actions

1. **NEVER** skip tests without SKIP:DOCKER or SKIP:INFRA reason
2. **NEVER** use `continue-on-error` without immediate analysis
3. **NEVER** claim "tests passed" without showing log evidence
4. **NEVER** truncate logs - read every line, every word
5. **NEVER** run tests without nuking cluster first
6. **NEVER** ignore "0 messages" or "0% delivery" in output
7. **NEVER** mask errors with try/except pass
8. **NEVER** use plaintext TCP for TLS-enabled server

---

## Activation Triggers

This skill activates when user mentions:
- "run all tests"
- "run tests" / "test suite"
- "verify no regressions"
- "full test run"
- "115+ tests"
- "chaos_dist"
- "TLS tests"
- "fix tests"
- "test stabilization"

---

## Example Session

```
User: "run all tests"

Agent:
1. [NUKE] Destroying all cluster state...
   $ sudo pkill -9 -f beam.smp; sudo pkill -9 epmd; ...
   Cluster nuked ✓

2. [START] Starting TLS-enabled server...
   $ erl -pa ebin -config config/test_tls ...
   Server listening on port 8085 with TLS ✓

3. [RUN] Executing test suite...
   $ python3 tests/run_tests.py --all 2>&1 | tee LOG_FILE
   Tests running...

4. [ANALYZE] Line-by-line log analysis...
   Lines: 5432, Errors: 0, Warnings: 2, Passes: 115, Skips: 0
   
   Warnings investigated:
   - Line 234: "deprecated function" → cosmetic, not affecting results
   - Line 1890: "timeout increased" → expected for slow test

5. [RESULT] Final status:
   ✓ ALL 115+ TESTS PASSED
   ✓ NO ERRORS DETECTED
   ✓ NO UNEXPLAINED SKIPS
   ✓ LOG ANALYSIS CLEAN
```

---

## Files Reference

| File | Purpose |
|------|---------|
| `tests/run_tests.py` | Main test runner with TLS enforcement |
| `tests/utilities/iris_client.py` | TLS-enabled Python client |
| `tests/suites/chaos_dist/utils.py` | TLS helpers for chaos tests |
| `config/test_tls.config` | TLS server configuration |
| `certs/ca.pem` | Certificate Authority for verification |
| `certs/edge-east-1.pem` | Server certificate |
