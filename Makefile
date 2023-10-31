all:

bench.encode:
	cd euneus_bench && mix encode

bench.decode:
	cd euneus_bench && mix decode

.PHONY: test

test:
	rebar3 do ct, eunit && cd euneus_test && mix test
