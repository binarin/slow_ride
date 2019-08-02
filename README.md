slow_ride
=====

[![Build Status](https://travis-ci.org/binarin/slow_ride.svg?branch=master)](https://travis-ci.org/binarin/slow_ride)

Local erlang distribution proxy for reliability testing. By providing its own epmd implementation, it connects all local erlang process through itself. Then those proxied connections can be inspected / throttled / broken.


Build
-----

    $ rebar3 compile
