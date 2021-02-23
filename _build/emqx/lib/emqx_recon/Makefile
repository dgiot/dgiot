
REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

REBAR := rebar3

all:
	$(REBAR) compile

eunit:
	$(REBAR) eunit -v

ct:
	$(REBAR) ct -v

xref:
	$(REBAR) xref

cover:
	$(REBAR) cover

dialyzer:
	$(REBAR) dialyzer

clean: distclean

distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock
