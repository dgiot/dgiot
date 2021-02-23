## shallow clone for speed

REBAR_GIT_CLONE_OPTIONS += --depth 1

export REBAR_GIT_CLONE_OPTIONS

REBAR = rebar3

all: compile

compile:
	$(REBAR) compile

proper: compile
	$(REBAR) as test proper -v -n 1000

ct: compile
	$(REBAR) as test ct -v

eunit: compile
	$(REBAR) as test eunit

test: ct proper eunit

tests: test

xref:
	$(REBAR) xref

cover:
	$(REBAR) cover

clean:
	$(REBAR) clean

distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock
