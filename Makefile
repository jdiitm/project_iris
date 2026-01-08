# Makefile for Project Iris
ERL = erl
ERLC = erlc
HOSTNAME := $(shell hostname -s)

ERL_FILES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl,ebin/%.beam,$(ERL_FILES))

APP_SRC = $(wildcard src/*.app.src)
APP_FILES = $(patsubst src/%.app.src,ebin/%.app,$(APP_SRC))

APPUP_SRC = $(wildcard src/*.appup.src)
APPUP_FILES = $(patsubst src/%.appup.src,ebin/%.appup,$(APPUP_SRC))

all: check_deps $(BEAM_FILES) $(APP_FILES) $(APPUP_FILES) test

ebin/%.app: src/%.app.src
	cp $< $@

ebin/%.appup: src/%.appup.src
	cp $< $@

ebin/%.beam: src/%.erl
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

start_core: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_core -eval "application:ensure_all_started(iris_core)" &

start_edge1: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge1 -iris_edge port 8085 -eval "application:ensure_all_started(iris_edge)" &

start_edge2: all
	$(ERL) -noshell -noinput $(ERL_FLAGS) -pa ebin -sname iris_edge2 -iris_edge port 8086 -eval "application:ensure_all_started(iris_edge)" &

stop:
	@echo "Stopping nodes..."
	@$(ERL) -noshell -sname stopper -eval "rpc:call('iris_edge2@$(HOSTNAME)', init, stop, []), \
	                                     rpc:call('iris_edge1@$(HOSTNAME)', init, stop, []), \
	                                     rpc:call('iris_core@$(HOSTNAME)', init, stop, []), \
	                                     init:stop()."
	@echo "Nodes stopped."
