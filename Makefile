all:

.PHONY: check

check: dialyzer test.erlang test.elixir

.PHONY: dialyzer

dialyzer:
	rebar3 dialyzer

.PHONY: test

test.erlang:
	rebar3 do ct, eunit

test.elixir:
	cd euneus_test && mix test

test: test.erlang test.elixir

.PHONY: bench

bench.encode:
	cd euneus_bench && mix encode

bench.decode:
	cd euneus_bench && mix decode
