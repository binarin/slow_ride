REBAR=rebar3

%:
	$(REBAR) $@

tests: eunit ct dialyzer
	echo OK
