#!/bin/sh

epmd -daemon
./rebar3 shell --sname=riak_ensemble_ng_demo --setcookie riak_ensemble_ng_demo_cookie
