all: deps

deps:
	rebar get-deps
	rebar compile

app:
	rebar compile

tests:
	rebar eunit

clean:
	rebar clean

distclean: clean
	rebar delete-deps

.PHONY: all deps app tests clean distclean