REBAR=rebar3

%:
	$(REBAR) $@

ct:
	$(REBAR) ct -v

tests: eunit ct dialyzer
	echo OK
