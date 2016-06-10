REBAR=rebar3

%:
	$(REBAR) $@

ct: release
	$(REBAR) ct -v

case:
	$(REBAR) ct -v --suite $(S) --case $(C) ; firefox $$(find -name *_suite.$(C).html | sort | tail -n1)

compile:
	$(REBAR) compile

tests: eunit ct dialyzer shell-test
	echo OK

shell-test: compile
	./scripts/shell-test.sh
