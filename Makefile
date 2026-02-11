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

# NIF: zstd compression (RFC Section 11.1)
ERL_INCLUDE = $(shell $(ERL) -noshell -eval 'io:format("~s", [code:root_dir()]), init:stop().')/usr/include
ERTS_INCLUDE = $(shell $(ERL) -noshell -eval 'io:format("~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), init:stop().')
NIF_SRC = c_src/iris_zstd_nif.c
NIF_SO = priv/iris_zstd_nif.so
NIF_CFLAGS = -shared -fPIC -O2 -I$(ERL_INCLUDE) -I$(ERTS_INCLUDE)
# ZSTD_INCLUDE and ZSTD_LIBDIR can be overridden for non-standard paths
ZSTD_INCLUDE ?=
ZSTD_LIBDIR ?=
NIF_ZSTD_CFLAGS = $(if $(ZSTD_INCLUDE),-I$(ZSTD_INCLUDE),)
NIF_ZSTD_LDFLAGS = $(if $(ZSTD_LIBDIR),-L$(ZSTD_LIBDIR),) -lzstd

all: check_deps nif $(BEAM_FILES) $(APP_FILES) $(APPUP_FILES)

# Build zstd NIF shared object (optional: requires libzstd-dev)
nif: $(NIF_SO)

$(NIF_SO): $(NIF_SRC)
	@mkdir -p priv
	@$(CC) $(NIF_CFLAGS) $(NIF_ZSTD_CFLAGS) -o $@ $< $(NIF_ZSTD_LDFLAGS) 2>/dev/null \
		&& echo "NIF: zstd compiled successfully" \
		|| echo "NIF: zstd skipped (libzstd-dev not installed — zstd compression unavailable)"

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
test-all: $(BEAM_FILES)
	@echo "Running all tests..."
	@./tests/run_all_tests.sh

# Run non-Docker tests only (faster)
test-quick: $(BEAM_FILES)
	@echo "Running quick tests (non-Docker)..."
	@./tests/run_all_tests.sh --quick

# Run Docker chaos tests only
test-docker-chaos: $(BEAM_FILES)
	@echo "Running Docker chaos tests..."
	@./tests/run_all_tests.sh --docker-only

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
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge1$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port $(or $(EDGE1_PORT),8085) -eval "application:ensure_all_started(iris_edge)" >edge1.log 2>&1 &

start_edge2: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge2$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port $(or $(EDGE2_PORT),8086) -eval "application:ensure_all_started(iris_edge)" >edge2.log 2>&1 &

start_edge3: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge3$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port $(or $(EDGE3_PORT),8087) -eval "application:ensure_all_started(iris_edge)" >edge3.log 2>&1 &

start_edge4: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge4$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port $(or $(EDGE4_PORT),8088) -eval "application:ensure_all_started(iris_edge)" >edge4.log 2>&1 &

start_edge5: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge5$(NODE_SUFFIX) -setcookie iris_secret -config $(CONFIG) -iris_edge port $(or $(EDGE5_PORT),8089) -eval "application:ensure_all_started(iris_edge)" >edge5.log 2>&1 &

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
