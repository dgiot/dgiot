REBAR:=./rebar3

.PHONY: all erl test clean doc hexp

all: erl

erl:
	$(REBAR)  compile

test: all
	$(REBAR) eunit

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit _build

hexp:
	MIX_EXS=package.exs mix hex.publish

doc:
	$(REBAR) doc

