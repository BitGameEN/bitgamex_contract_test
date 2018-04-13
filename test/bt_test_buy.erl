%%%-------------------------------------------------------------------
%%% File    : bt_test_buy.erl
%%% Description : buy testing
%%%-------------------------------------------------------------------
-module(bt_test_buy).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("record.hrl").

-record(context, {
}).

buy_test_() ->
    {setup,
        fun() -> start() end,
        fun(Context) -> stop(Context) end,
        {with, [
            fun buy/1
        ]}
    }.

start() ->
    ?debugFmt("test buy begin......~n", []),
    #context{}.

stop(#context{}) ->
    ?debugFmt("test buy end.~n", []).

buy(#context{}) ->
    ?assertMatch(ok, ok).

