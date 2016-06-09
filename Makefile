REBAR=rebar3

%:
	$(REBAR) $@

ct:
	$(REBAR) ct -v

case:
	$(REBAR) ct -v --suite $(S) --case $(C)
	firefox $$(find -name *_suite.$(C).html | sort | tail -n1)

tests: eunit ct dialyzer
	echo OK
