# Auto-detect a valid Erlang executable that has Mnesia
ERL_CANDIDATES := erl /usr/bin/erl
ERL := $(shell for e in $(ERL_CANDIDATES); do if $$e -noshell -eval 'case code:lib_dir(mnesia) of {error,_}->halt(1);_->halt(0) end' 2>/dev/null; then echo $$e; break; fi; done)

# Fallback if no valid Erlang found (check_deps will fail later with details)
ifeq ($(ERL),)
ERL := erl
endif

ERLC ?= erlc
HOSTNAME := $(shell hostname -s)

ERL_FILES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl,ebin/%.beam,$(ERL_FILES))

all: check_deps $(BEAM_FILES)

ebin/%.beam: src/%.erl
	$(ERLC) -o ebin $<

check_deps:
	@$(ERL) -noshell -eval 'case code:lib_dir(mnesia) of {error, _} -> io:format("Error: mnesia application not found in Erlang lib (~s).~n", [code:root_dir()]), init:stop(1); _ -> init:stop(0) end.' || (echo "FAILED: Valid Erlang with Mnesia not found. Please set ERL variable." && exit 1)

clean:
	rm -f ebin/*.beam

start_core: all
	$(ERL) -detached -pa ebin -sname iris_core -eval "application:ensure_all_started(iris_core)"

start_edge1: all
	$(ERL) -detached -pa ebin -sname iris_edge1 -iris_edge port 8085 -eval "application:ensure_all_started(iris_edge)"

start_edge2: all
	$(ERL) -detached -pa ebin -sname iris_edge2 -iris_edge port 8086 -eval "application:ensure_all_started(iris_edge)"
