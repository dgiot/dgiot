## shallow clone for speed

REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

ct: compile
	$(REBAR) as test ct -v --name emqx_exproto_ct@127.0.0.1

eunit: compile
	$(REBAR) as test eunit

xref:
	$(REBAR) xref

proper:
	$(REBAR) proper -d test

dialyzer:
	$(REBAR) dialyzer

cover:
	$(REBAR) cover

distclean: clean
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock

compile-priv:
	@for folder in $$(ls -1 priv); do \
        (cd priv/$$folder; $(MAKE)) \
    done
	@mkdir -p ebin

priv-clean:
	@for folder in $$(ls -1 priv); do \
        (cd priv/$$folder; $(MAKE) clean) \
    done

CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

$(CUTTLEFISH_SCRIPT):
	@${REBAR} get-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

app.config: $(CUTTLEFISH_SCRIPT) etc/emqx_exproto.conf
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/emqx_exproto.conf -i priv/emqx_exproto.schema -d data

