%%%--------------------------------------
%%% @Module  : bt
%%% @Description:  服务开启
%%%--------------------------------------
-module(bt).
-export([test_start/0, test_stop/0]).

-define(TEST_APPS, [sasl, bitgamex_contract_test, gun]).

-include("common.hrl").


%%启动test
test_start() ->
    try
        ok = application:start(crypto),
        % ssl
        ok = application:start(asn1),
        ok = application:start(public_key),
        ok = application:start(ssl),
        % ssl-end
        ok = lager:start(),
        ok = application:start(ranch),
        ok = application:start(cowlib),
        ok = start_applications(?TEST_APPS)
    after
        timer:sleep(100)
    end.

%%停止test
test_stop() ->
    try
        ok = stop_applications(?TEST_APPS)
    after
        init:stop() % 能停掉heart
    end.

%%############辅助调用函数##############
manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];%合拢
                        {error, {SkipError, _}} -> Acc;
                        {error, Reason} ->
                            [Undo(One) || One <- Acc],
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        already_started,
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).
