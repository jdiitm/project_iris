# Makefile for Project Iris
ERL ?= $(shell which erl 2>/dev/null || echo erl)
ERLC ?= $(shell which erlc 2>/dev/null || echo erlc)
HOSTNAME := $(shell hostname -s)

SRC_FILES = $(filter-out %_tests.erl, $(wildcard src/*.erl))
UTIL_FILES = $(wildcard test_utils/*.erl)
BEAM_FILES = $(patsubst src/%.erl,ebin/%.beam,$(SRC_FILES)) $(patsubst test_utils/%.erl,ebin/%.beam,$(UTIL_FILES))

APP_SRC = $(wildcard src/*.app.src)
APP_FILES = $(patsubst src/%.app.src,ebin/%.app,$(APP_SRC))

APPUP_SRC = $(wildcard src/*.appup.src)
APPUP_FILES = $(patsubst src/%.appup.src,ebin/%.appup,$(APPUP_SRC))

all: check_deps $(BEAM_FILES) $(APP_FILES) $(APPUP_FILES)

ebin/%.app: src/%.app.src
	cp $< $@

ebin/%.appup: src/%.appup.src
	cp $< $@

ebin/%.beam: src/%.erl
	$(ERLC) -o ebin $<

ebin/%.beam: test_utils/%.erl
	$(ERLC) -o ebin $<

check_deps:
	@$(ERL) -noshell -eval 'case code:lib_dir(mnesia) of {error, _} -> io:format("Error: mnesia application not found in Erlang lib (~s).~n", [code:root_dir()]), init:stop(1); _ -> init:stop(0) end.' || (echo "FAILED: Valid Erlang with Mnesia not found. Please set ERL variable." && exit 1)

# Run unit tests
test: $(BEAM_FILES)
	@echo "Running EUnit tests..."
	@$(ERL) -pa ebin -noshell -eval "case eunit:test([iris_session_tests, iris_proto_tests, iris_shard_tests, iris_ingress_guard_tests], []) of ok -> init:stop(0); error -> init:stop(1) end."

# Run tests with verbose output
test-verbose: $(BEAM_FILES)
	@echo "Running EUnit tests (verbose)..."
	@$(ERL) -pa ebin -noshell -eval "case eunit:test([iris_session_tests, iris_proto_tests, iris_shard_tests, iris_ingress_guard_tests], [verbose]) of ok -> init:stop(0); error -> init:stop(1) end."

# Run all tests via unified test runner
# Uses stdbuf -oL to force line-buffered output for real-time visibility
test-all: $(BEAM_FILES)
	@echo "Running all tests..."
	@stdbuf -oL python3 -u tests/run_tests.py --all

# Run CI Tier 0 tests (required on every merge)
test-tier0: $(BEAM_FILES)
	@echo "Running Tier 0 tests..."
	@stdbuf -oL python3 -u tests/run_tests.py --tier 0

# Run CI Tier 1 tests (nightly/manual)
test-tier1: $(BEAM_FILES)
	@echo "Running Tier 1 tests..."
	@stdbuf -oL python3 -u tests/run_tests.py --tier 1

# Run integration tests only
test-integration: $(BEAM_FILES)
	@echo "Running integration tests..."
	@stdbuf -oL python3 -u tests/run_tests.py --suite integration

# List available tests
test-list:
	@python3 tests/run_tests.py --list

# Run property-based tests (PropEr-style)
test-proper: $(BEAM_FILES)
	@echo "Running property-based tests..."
	@$(ERL) -pa ebin -noshell -eval "case iris_proto_props:test_all() of ok -> init:stop(0); error -> init:stop(1) end."

clean:
	rm -f ebin/*.beam


# Auto-tune: Calculate optimal flags
ERL_FLAGS := $(shell ./scripts/auto_tune.sh)

# Config file (without .config extension)
CONFIG ?= config/test

# Start both core and edge nodes
start: start_core start_edge1

start_core: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_core$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -eval "application:ensure_all_started(iris_core)" >core.log 2>&1 &

start_edge1: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge1$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port 8085 -eval "application:ensure_all_started(iris_edge)" >edge1.log 2>&1 &

start_edge2: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge2$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port 8086 -eval "application:ensure_all_started(iris_edge)" >edge2.log 2>&1 &

start_edge3: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge3$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port 8087 -eval "application:ensure_all_started(iris_edge)" >edge3.log 2>&1 &

start_edge4: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge4$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port 8088 -eval "application:ensure_all_started(iris_edge)" >edge4.log 2>&1 &

start_edge5: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge5$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port 8089 -eval "application:ensure_all_started(iris_edge)" >edge5.log 2>&1 &

# ... (Previous targets)

# Distributed Cluster Targets (Public Cloud / Hybrid)
# Usage: make start_core_dist NAME=iris_core1@laptop-a COOKIE=secret CONFIG=config/prod
start_core_dist: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -name $(NAME) -setcookie $(COOKIE) -config $(CONFIG) -eval "application:ensure_all_started(iris_core)" >core.log 2>&1 &

# Usage: make start_edge_dist NAME=iris_edge1@cloud-vm COOKIE=secret CONFIG=config/prod
start_edge_dist: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -name $(NAME) -setcookie $(COOKIE) -config $(CONFIG) -eval "application:ensure_all_started(iris_edge)" >edge.log 2>&1 &

stop:
	@echo "Stopping nodes..."
	@-pkill -f "beam.smp.*iris_" 2>/dev/null; true
	@echo "Nodes stopped."

# =============================================================================
# Global Cluster Simulation (Docker)
# =============================================================================
cluster-up: all
	@echo "Starting 5-region global cluster..."
	@docker/global-cluster/cluster.sh up

cluster-mtls: all certs
	@echo "Starting 5-region global cluster with mTLS..."
	@docker/global-cluster/cluster.sh up-mtls

cluster-chaos: all
	@echo "Starting global cluster with chaos injection..."
	@docker/global-cluster/cluster.sh up-chaos

cluster-chaos-mtls: all certs
	@echo "Starting global cluster with chaos + mTLS..."
	@docker/global-cluster/cluster.sh up-chaos-mtls

cluster-down:
	@echo "Stopping global cluster..."
	@docker/global-cluster/cluster.sh down

cluster-down-mtls:
	@echo "Stopping mTLS cluster..."
	@docker/global-cluster/cluster.sh down-mtls

cluster-verify-mtls:
	@docker/global-cluster/cluster.sh verify-mtls

cluster-status:
	@docker/global-cluster/cluster.sh status

cluster-clean:
	@docker/global-cluster/cluster.sh clean

# Generate mTLS certificates
certs:
	@if [ ! -f certs/ca.pem ]; then \
		echo "Generating mTLS certificates..."; \
		cd certs && bash generate_certs.sh; \
	else \
		echo "Certificates already exist. Use 'make certs-clean' to regenerate."; \
	fi

certs-clean:
	@echo "Removing certificates..."
	@rm -f certs/*.pem certs/*.key certs/*.srl 2>/dev/null || true
# =============================================================================
# Comprehensive Test Runner
# =============================================================================

# Run all 88 tests with Docker cluster
test-all-docker:
	@echo "Running comprehensive test suite (88 tests)..."
	@./scripts/run_all_tests.sh

# Quick test run (skip slow tests)
test-quick:
	@echo "Running quick test suite..."
	@./scripts/run_all_tests.sh --quick

# Verify test setup
test-verify:
	@echo "Verifying test infrastructure..."
	@docker ps | grep -q "edge-east-1" || (echo "Docker cluster not running. Run: make cluster-up" && exit 1)
	@python3 -c "import socket; s=socket.socket(); s.settimeout(2); s.connect(('localhost', 8085)); s.close()" || (echo "Cannot connect to edge node" && exit 1)
	@[ -f certs/ca.pem ] || (echo "Certificates missing. Run: make certs" && exit 1)
	@echo "✓ All prerequisites met"

# =============================================================================
# Deterministic Docker Test Environment
# =============================================================================

# Run all tests in Docker (fully deterministic)
test-docker:
	@echo "Running tests in Docker (seed=$(TEST_SEED:-42))..."
	docker build -t iris-test -f docker/test/Dockerfile .
	docker run --rm \
		-e TEST_SEED=$${TEST_SEED:-42} \
		-e CI=true \
		iris-test

# Run tests with custom seed for reproduction
test-docker-seed:
	@echo "Running tests with seed=$(SEED)..."
	docker run --rm \
		-e TEST_SEED=$(SEED) \
		-e CI=true \
		iris-test

# Run specific suite in Docker
test-docker-suite:
	@echo "Running suite $(SUITE) in Docker..."
	docker run --rm \
		-e TEST_SEED=$${TEST_SEED:-42} \
		-e CI=true \
		iris-test \
		python3 tests/run_tests.py --suite $(SUITE)

# Run full cluster tests in Docker
test-docker-cluster:
	@echo "Running full cluster tests..."
	docker-compose -f docker/test/docker-compose.test.yml up --build --abort-on-container-exit
	docker-compose -f docker/test/docker-compose.test.yml down -v

# Run cross-region latency test (requires Docker global cluster)
test-cross-region: cluster-up
	@echo "Running cross-region latency test..."
	@echo "Waiting for cluster to stabilize (30s)..."
	@sleep 30
	@python3 tests/suites/chaos_dist/test_cross_region_latency.py
	@echo "Cross-region test complete"

# =============================================================================
# Cluster-Dependent Tests (chaos_dist)
# =============================================================================

# Run all cluster-dependent tests (cross-region, multi-master durability)
# This target manages the full cluster lifecycle
test-cluster-dist: all
	@echo "=============================================="
	@echo "Running Cluster-Dependent Tests"
	@echo "=============================================="
	@echo ""
	@echo "Step 1: Starting Docker global cluster..."
	@docker/global-cluster/cluster.sh up
	@echo ""
	@echo "Step 2: Verifying cluster readiness..."
	@python3 scripts/verify_cluster_ready.py --quick || (echo "Cluster not ready!" && make cluster-down && exit 1)
	@echo ""
	@echo "Step 3: Running cross-region latency test..."
	@python3 tests/suites/chaos_dist/test_cross_region_latency.py || true
	@echo ""
	@echo "Step 4: Running multi-master durability test..."
	@python3 tests/suites/chaos_dist/test_multimaster_durability.py || true
	@echo ""
	@echo "Step 5: Running ack durability test..."
	@python3 tests/suites/chaos_dist/test_ack_durability.py || true
	@echo ""
	@echo "Step 6: Stopping cluster..."
	@make cluster-down
	@echo ""
	@echo "=============================================="
	@echo "Cluster-Dependent Tests Complete"
	@echo "=============================================="

# Verify cluster is ready for tests
cluster-verify:
	@python3 scripts/verify_cluster_ready.py

# Quick cluster verification (skip cross-region delivery test)
cluster-verify-quick:
	@python3 scripts/verify_cluster_ready.py --quick

# Clean Docker test artifacts
test-docker-clean:
	@echo "Cleaning Docker test artifacts..."
	-docker rmi iris-test 2>/dev/null
	-docker-compose -f docker/test/docker-compose.test.yml down -v 2>/dev/null
	@echo "Docker test artifacts cleaned"
