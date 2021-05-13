REBAR := rebar3

CT_NODE_NAME = ct@127.0.0.1

.PHONY: all
all: compile

compile:
	$(REBAR) compile

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock

.PHONY: xref
xref:
	$(REBAR) xref

.PHONY: eunit
eunit: compile
	$(REBAR) eunit verbose=true

.PHONY: ct
ct: compile
	$(REBAR) as test ct -v --readable=false --name $(CT_NODE_NAME)

cover:
	$(REBAR) cover

.PHONY: coveralls
coveralls:
	@rebar3 as test coveralls send

.PHONY: dialyzer
dialyzer:
	$(REBAR) dialyzer

CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

$(CUTTLEFISH_SCRIPT):
	@${REBAR} get-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

app.config: $(CUTTLEFISH_SCRIPT)
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/ekka.conf.example -i priv/ekka.schema -d data/
