all:

bench.encode:
	cd euneus_bench && mix encode

bench.encode_opts:
	cd euneus_bench && mix encode_opts

bench.decode:
	cd euneus_bench && mix decode

bench.decode_opts:
	cd euneus_bench && mix decode_opts

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
