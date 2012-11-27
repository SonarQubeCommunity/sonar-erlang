PATH := ${PATH}:/home/tkende/dev/erlang/rebar

all: clean compile dialyzer test

# Compiles the whole application
compile:
	rebar compile
	rebar skip_deps=true xref | grep -v "is unused export (Xref)"

# Cleans any generated files from the repo (except dependencies)
clean:
	rebar clean
	rm -f test/ebin/*
	rm -f doc/*.html doc/*.css doc/*.png doc/edoc-info

# Runs every test suite under test/ abd generates an html page with detailed info about test coverage
test: compile
	rebar skip_deps=true eunit

# Generates the edoc documentation and places it under doc/ .
docs:
	rebar skip_deps=true doc

# Launches an erlang shell where the deps and the modules from the project are accesible
shell: compile
	rebar shell

dialyzer:
	dialyzer ebin -o  .eunit/dialyzer.log
