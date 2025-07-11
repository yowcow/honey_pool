REBAR3 := rebar3

all:
	$(REBAR3) compile

test:
	$(REBAR3) do eunit,xref,dialyzer

format:
	$(REBAR3) efmt -w -- src/** test/**

.PHONY: all test format
