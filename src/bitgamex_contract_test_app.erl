%%%-----------------------------------
%%% @Module  : bitgamex_contract_test_app
%%% @Description: 
%%%-----------------------------------
-module(bitgamex_contract_test_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("common.hrl").
-include("record.hrl").

start(normal, []) ->
    ?INFO("====> bitgamex contract test node launching...Please wait...~n", []),
    {ok, SupPid} = bitgamex_contract_test_sup:start_link(),
    bitgamex_contract_test:start([]),
    {ok, SupPid}.

stop(_State) ->
    void.

