-module(buy_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [{group, buy}].

groups() ->
    [{buy, [parallel], ct_helper:all(?MODULE)}].

init_per_group(buy, Config) ->
    % 启动test
    bt:test_start(),
    % 启动geth
    ct:log("starting geth...", []),
    spawn(fun() -> os:cmd("geth --rpc --rpcapi eth,net,web3,personal --rpcport 8545 --dev --minerthreads 1 --ipcpath /Users/guli1/Library/Ethereum/geth.ipc") end),
    ct:log("waiting 5 seconds...", []),
    timer:sleep(5000),
    % 创建100个账号
    ct:log("creating 100 accounts...", []),
    L = [begin
             {ok, Addr} = erthereum:personal_newAccount(<<"test">>),
             ct:log("account ~p created: ~p~n", [I, Addr]),
             {I, Addr}
         end || I <- lists:seq(1, 100)],
    [{accounts, L} | Config];
init_per_group(_Name, Config) ->
    Config.

end_per_group(_Name, _Config) ->
    % 停止geth
    os:cmd("ps -efww|grep \"geth\"|grep -v grep|grep -v attach|cut -d ' ' -f 4|xargs kill -9"),
    % 停止test
    bt:test_stop(),
    ok.

test_case1(Config) ->
    ok.

