#!/bin/sh

./elvis rock && rebar3 do xref, dialyzer, eunit
