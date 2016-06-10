%:
	rebar3 $@

ct: release
	rebar3 ct -v

case:
	rebar3 ct -v --suite $(S) --case $(C) ; firefox $$(find -name *_suite.$(C).html | sort | tail -n1)

compile:
	rebar3 compile

tests: eunit ct dialyzer shell-test
	echo OK

shell-test: compile
	./scripts/shell-test.sh
