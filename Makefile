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
	@$(ERL) -pa ebin -noshell -eval "case eunit:test([iris_session_tests, iris_proto_tests], []) of ok -> init:stop(0); error -> init:stop(1) end."

# Run tests with verbose output
test-verbose: $(BEAM_FILES)
	@echo "Running EUnit tests (verbose)..."
	@$(ERL) -pa ebin -noshell -eval "case eunit:test([iris_session_tests, iris_proto_tests], [verbose]) of ok -> init:stop(0); error -> init:stop(1) end."

# Run all tests via unified test runner
test-all: $(BEAM_FILES)
	@echo "Running all tests..."
	@python3 tests/run_tests.py --all

# Run CI Tier 0 tests (required on every merge)
test-tier0: $(BEAM_FILES)
	@echo "Running Tier 0 tests..."
	@python3 tests/run_tests.py --tier 0

# Run CI Tier 1 tests (nightly/manual)
test-tier1: $(BEAM_FILES)
	@echo "Running Tier 1 tests..."
	@python3 tests/run_tests.py --tier 1

# Run integration tests only
test-integration: $(BEAM_FILES)
	@echo "Running integration tests..."
	@python3 tests/run_tests.py --suite integration

# List available tests
test-list:
	@python3 tests/run_tests.py --list

clean:
	rm -f ebin/*.beam


# Auto-tune: Calculate optimal flags
ERL_FLAGS := $(shell ./scripts/auto_tune.sh)

# Config file (without .config extension)
CONFIG ?= config/test

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
