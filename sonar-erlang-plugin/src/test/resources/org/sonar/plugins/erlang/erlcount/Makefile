PATH := ${PATH}:/home/tkende/dev/erlang/rebar

all: clean compile test dialyzer

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

getdeps:
	rebar get-deps

### Dialyzer / static-analysis stuff ###

DIALYZER_LOG_FILE ?= $(PWD)/.eunit/dialyzer.log
DIALYZER_REPORT_ARG := -o $(DIALYZER_LOG_FILE)


DIALYZER_OTP_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools \
crypto inets xmerl webtool snmp public_key mnesia eunit syntax_tools \
compiler

OTP_PLT ?= $(HOME)/.dialyzer_plt
LOCAL_PLT ?= $(PWD)/.dialyzer_plt

$(OTP_PLT):
	dialyzer --build_plt --apps $(DIALYZER_OTP_APPS) --output_plt $(OTP_PLT)

$(LOCAL_PLT): $(OTP_PLT)
	dialyzer --add_to_plt \
		--plt $(OTP_PLT) \
		--output_plt $(LOCAL_PLT)

dialyzer: $(LOCAL_PLT)
	rebar co skip_deps=true
	dialyzer --plt $(LOCAL_PLT) \
		-c $(PWD)/ebin \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		$(DIALYZER_REPORT_ARG)
