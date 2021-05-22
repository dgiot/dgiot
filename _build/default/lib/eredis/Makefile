APP=eredis

PRE17 := $(shell ERL_FLAGS="" erl -eval 'io:format("~s~n", [case re:run(erlang:system_info(otp_release), "^R") of nomatch -> ""; _ -> pre17 end]), halt().' -noshell)

.PHONY: all compile clean Emakefile

all: compile

compile: ebin/$(APP).app Emakefile
	erl -noinput -eval 'up_to_date = make:all()' -s erlang halt

clean:
	rm -f -- ebin/*.beam Emakefile ebin/$(APP).app

ebin/$(APP).app: src/$(APP).app.src
	mkdir -p ebin
	cp -f -- $< $@

ifdef DEBUG
EXTRA_OPTS:=debug_info,
endif

ifdef TEST
EXTRA_OPTS:=$(EXTRA_OPTS) {d,'TEST', true},
endif

ifndef PRE17
EXTRA_OPTS:=$(EXTRA_OPTS) {d,namespaced_types},
endif

Emakefile: Emakefile.src
	sed "s/{{EXTRA_OPTS}}/$(EXTRA_OPTS)/" $< > $@

