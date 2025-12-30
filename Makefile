ebin/%.beam: src/%.erl
	/usr/bin/erlc -o ebin $<

ERL_FILES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl,ebin/%.beam,$(ERL_FILES))

all: $(BEAM_FILES)

clean:
	rm -f ebin/*.beam

start_core: all
	/usr/bin/erl -detached -pa ebin -sname iris_core -eval "application:ensure_all_started(iris_core)"

start_edge1: all
	/usr/bin/erl -detached -pa ebin -sname iris_edge1 -iris_edge port 8085 -eval "application:ensure_all_started(iris_edge)"

start_edge2: all
	/usr/bin/erl -detached -pa ebin -sname iris_edge2 -iris_edge port 8086 -eval "application:ensure_all_started(iris_edge)"
