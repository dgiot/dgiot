# See LICENSE for licensing information.

DIALYZER = dialyzer
REBAR = rebar
APPNAME = goldrush

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app eunit ct

eunit:
	@$(REBAR) -C rebar.test.config eunit skip_deps=true

ct:
	@$(REBAR) -C rebar.test.config ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(APPNAME)_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl compiler syntax_tools

dialyze:
	@$(DIALYZER) --src src --plt .$(APPNAME)_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
