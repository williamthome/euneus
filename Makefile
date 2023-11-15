all:

bench.encode:
	cd euneus_bench && mix encode

bench.decode:
	cd euneus_bench && mix decode

.PHONY: test

test.rebar3:
	rebar3 as test do ct, eunit

test.mix:
	cd euneus_test && mix test

test: test.rebar3 test.mix

.PHONY: check

check: test
	rebar3 dialyzer

.PHONY: publish

publish: check
	rebar3 hex publish
