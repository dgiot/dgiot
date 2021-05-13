## shallow clone for speed

REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

ct: compile
	$(REBAR) as test ct -v

eunit: compile
	$(REBAR) as test eunit

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

proper:
	$(REBAR) proper -d test/props

cover:
	$(REBAR) cover

clean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock

CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

$(CUTTLEFISH_SCRIPT):
	@${REBAR} get-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

app.config: $(CUTTLEFISH_SCRIPT) etc/emqx_web_hook.conf
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/emqx_web_hook.conf -i priv/emqx_web_hook.schema -d data
