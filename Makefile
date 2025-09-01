.PHONY: test

REBAR ?= rebar3

test:
	clear
	${REBAR} eunit
	${REBAR} ct