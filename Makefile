# Makefile for Project Iris
ERL = /usr/bin/erl
ERLC = /usr/bin/erlc
HOSTNAME := $(shell hostname -s)

ERL_FILES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl,ebin/%.beam,$(ERL_FILES))

APP_SRC = $(wildcard src/*.app.src)
APP_FILES = $(patsubst src/%.app.src,ebin/%.app,$(APP_SRC))

all: check_deps $(BEAM_FILES) $(APP_FILES)

ebin/%.app: src/%.app.src
	cp $< $@

ebin/%.beam: src/%.erl
	$(ERLC) -o ebin $<

check_deps:
	@$(ERL) -noshell -eval 'case code:lib_dir(mnesia) of {error, _} -> io:format("Error: mnesia application not found in Erlang lib (~s).~n", [code:root_dir()]), init:stop(1); _ -> init:stop(0) end.' || (echo "FAILED: Valid Erlang with Mnesia not found. Please set ERL variable." && exit 1)

clean:
	rm -f ebin/*.beam


# Auto-tune: Calculate optimal flags
ERL_FLAGS := $(shell ./scripts/auto_tune.sh)

start_core: all
	$(ERL) -detached $(ERL_FLAGS) -pa ebin -sname iris_core -eval "application:ensure_all_started(iris_core)"

start_edge1: all
	$(ERL) -detached $(ERL_FLAGS) -pa ebin -sname iris_edge1 -iris_edge port 8085 -eval "application:ensure_all_started(iris_edge)"

start_edge2: all
	$(ERL) -detached $(ERL_FLAGS) -pa ebin -sname iris_edge2 -iris_edge port 8086 -eval "application:ensure_all_started(iris_edge)"

stop:
	@echo "Stopping nodes..."
	@$(ERL) -noshell -sname stopper -eval "rpc:call('iris_edge2@$(HOSTNAME)', init, stop, []), \
	                                     rpc:call('iris_edge1@$(HOSTNAME)', init, stop, []), \
	                                     rpc:call('iris_core@$(HOSTNAME)', init, stop, []), \
	                                     init:stop()."
	@echo "Nodes stopped."
