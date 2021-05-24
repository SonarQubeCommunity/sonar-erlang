.DEFAULT_GOAL = compile

DIR = ${PWD}
REBAR := rebar3
CT_COVER = -cover $(DIR)/config/test.cover.spec
TEST_DEPS_PATH = $(DIR)/_build/test/lib/*/ebin

all: clean compile dialyzer eunit

# Compiles the whole application
compile:
	$(REBAR) compile
	$(REBAR) xref | grep -v "is unused export (Xref)"

# Cleans any generated files from the repo (except dependencies)
clean:
	$(REBAR) clean
	rm -f test/ebin/*
	rm -f doc/*.html doc/*.css doc/*.png doc/edoc-info

ct_run:
	rm -rf $(DIR)/test/*.beam $(DIR)/test/*/*.beam $(DIR)/log/*
	$(REBAR) as test compile
	ct_run -dir $(DIR)/test -pa $(TEST_DEPS_PATH) -v -logdir $(DIR)/log $(CT_COVER)

# Runs every test suite under test/ abd generates an html page with detailed info about test coverage
eunit: compile
	$(REBAR) eunit

# Generates the edoc documentation and places it under doc/ .
docs:
	$(REBAR) doc

# Launches an erlang shell where the deps and the modules from the project are accesible
shell: compile
	$(REBAR) shell

getdeps:
	$(REBAR) get-deps

dialyzer:
	dialyzer ebin -o  .eunit/dialyzer.log
