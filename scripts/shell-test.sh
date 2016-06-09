#!/usr/bin/env bash
set -eu
set -o pipefail

ROOT=$(readlink -f $(dirname $0)/..)

R=$ROOT/scripts/slow_ride

wait-start() {
    echo Waiting for slow_ride registration
    local try_no
    for try_no in $(seq 1 20); do
        if epmd -names | grep -qP 'name slow_ride at'; then
            return 0
        fi
        sleep 1
    done
    return 1
}

run-epmd() {
    ERL_EPMD_PORT=44369 epmd "$@"
}

check-port-action() {
    echo "Testing 'port' command"
    local port
    port=$(action port)
    if [[ $port != 44369 ]]; then
        echo Got unexpected port "'$port'"
        return 1
    fi
}

check-stop-action() {
    echo "Testing 'stop' command"
    local try_no
    pgrep -f 'beam.*-sname slow_ride@localhost' > /dev/null
    sleep 1
    action stop
    for try_no in $(seq 1 20); do
        if ! pgrep -f 'beam.*-sname slow_ride@localhost' > /dev/null; then
            return 0
        fi
        sleep 1
    done
    echo "Failed to stop slow_ride"
    return 1
}

action() {
    $R "$@"
}

ensure-epmd() {
    erl -noinput -sname epmd-start -s erlang halt
}


start-slow-ride() {
    echo Starting slow_ride
    $R start > /dev/null 2>&1 &
    wait-start
}

ensure-epmd
start-slow-ride
check-port-action
check-stop-action



