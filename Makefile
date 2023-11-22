all:

bench.encode:
	cd euneus_bench && mix encode

bench.encode_v2:
	cd euneus_bench && mix encode_v2

bench.encode_parsed:
	cd euneus_bench && mix encode_parsed

bench.encode_parsed_codecs:
	cd euneus_bench && mix encode_parsed_codecs

bench.decode:
	cd euneus_bench && mix decode

bench.decode_v2:
	cd euneus_bench && mix decode_v2

bench.decode_parsed:
	cd euneus_bench && mix decode_parsed

bench.decode_parsed_codecs:
	cd euneus_bench && mix decode_parsed_codecs

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
