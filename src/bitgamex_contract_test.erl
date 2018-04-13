%%%-----------------------------------
%%% @Module  : bitgamex_contract_test
%%% @Description: test node启动
%%%-----------------------------------
-module(bitgamex_contract_test).
-export([start/1]).
-include("common.hrl").

start([]) ->
    ok = start_kernel(),
    ?INFO("<==== bitgamex contract test node ready.~n~n~n", []),
    ok.

%%开启核心服务
start_kernel() ->
    ?INFO("====> start_kernel...~n", []),
    {ok,_} = supervisor:start_child(
                bitgamex_contract_test_sup,
                {mod_kernel,
                {mod_kernel, start_link, []},
                permanent, 10000, supervisor, [mod_kernel]}),
    ok.

