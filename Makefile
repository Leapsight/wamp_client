.PHONY: test

REBAR ?= rebar3

export CPATH=/opt/homebrew/include
export LIBRARY_PATH=/opt/homebrew/lib

test:
	clear
	${REBAR} test