.PHONY: test

REBAR ?= rebar3

test:
	clear
	${REBAR} test