%%%------------------------------------
%%% @Module  : mod_kernel
%%% @Description: 核心服务（ETS表的holder）
%%%------------------------------------
-module(mod_kernel).
-behaviour(gen_server).
-export([
            start_link/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("record.hrl").

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    %%初始ets表
    ?INFO("====> \tinit_ets...~n", []),
    ok = init_ets(),
    {ok, 1}.

handle_cast(_R , Status) ->
    {noreply, Status}.

handle_call(_R , _FROM, Status) ->
    {reply, ok, Status}.

handle_info(_Info, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

%% ================== 私有函数 =================

%%初始ETS表
init_ets() ->
    ok.

