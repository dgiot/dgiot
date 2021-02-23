DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	public_key mnesia syntax_tools compiler
COMBO_PLT = $(HOME)/.cuttlefish_combo_dialyzer_plt

REBAR := $(CURDIR)/rebar3

REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

.PHONY: all
all: $(REBAR) compile
	echo $(REBAR)

.PHONY: deps
deps:
	$(REBAR) get-deps

.PHONY: distclean
docsclean:
	@rm -rf doc/*.png doc/*.html doc/*.css doc/edoc-info

.PHONY: compile
compile: deps
	$(REBAR) compile

## Environment variable CUTTLEFISH_ESCRIPT is shared with rebar.config
.PHONY: escript
escript: export CUTTLEFISH_ESCRIPT = true
escript: $(REBAR)
	$(REBAR) as escript escriptize

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build cuttlefish erl_crash.dump rebar3.crashdump rebar.lock

.PHONY: eunit
eunit: compile
	$(REBAR) eunit verbose=true

.PHONY: dialyzer
dialyzer:
	$(REBAR) dialyzer

.PHONY: $(REBAR)
$(REBAR):
ifneq ($(wildcard rebar3),rebar3)
	@curl -Lo rebar3 $(REBAR_URL) || wget $(REBAR_URL)
endif
	@chmod a+x rebar3

ifeq ($(OS),Windows_NT)
	@$(CURDIR)/rebar3 update
endif