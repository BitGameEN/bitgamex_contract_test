%%%-------------------------------------------------------------------
%%% File    : bt_test_all.erl
%%% Description : run all test functions
%%%-------------------------------------------------------------------
-module(bt_test_all).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    [bt_test_buy].
