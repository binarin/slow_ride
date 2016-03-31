REBAR=rebar3

%:
	$(REBAR) $@

tests:
	$(REBAR) eunit
