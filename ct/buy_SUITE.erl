-module(buy_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [{group, buy}].

groups() ->
    [{buy, [parallel], [test_case_normal]}].

init_per_suite(Config) ->
    % 启动test
    bt:test_start(),
    Config.

end_per_suite(Config) ->
    % 停止test
    bt:test_stop(),
    ok.

init_per_group(buy, Config) ->
    % 启动geth
    ct:log("starting geth...", []),
    % 先停止geth
    os:cmd("ps -efww|grep \"geth\"|grep -v grep|grep -v attach|tr -s ' '|cut -d ' ' -f 3|xargs kill -9"),
    % 如果要区块高度一直增高，启动geth时加入--dev.period 1（但会白耗CPU），否则只靠交易驱动挖矿
    spawn(fun() -> os:cmd("mkdir eth; geth --datadir ./eth/ --rpc --rpcapi eth,net,web3,personal --rpcport 8545 --dev --mine --minerthreads 1 --ipcpath /Users/guli1/Library/Ethereum/geth.ipc 2>>./eth/eth.log") end),
    ct:log("waiting 5 seconds...", []),
    timer:sleep(5000),
    % 创建100个账号
    ct:log("creating 100 accounts...", []),
    L = [begin
             {ok, Addr} = erthereum:personal_newAccount(<<"test">>),
             ct:log("account ~p created: ~p~n", [I, Addr]),
             {I, Addr}
         end || I <- lists:seq(1, 100)],
    % 发布智能合约
    ct:log("deploying contract...", []),
    {ok, ContractAddr} = deploy_contract(L),
    % 主账号向100个账号划拨ETH
    distribute_ETH(L),
    [{accounts, L}, {contract_addr, ContractAddr} | Config];
init_per_group(_Name, Config) ->
    Config.

end_per_group(_Name, _Config) ->
    % 停止geth（放在初始化）
    ok.

test_case_normal(Config) ->
    % 先检查各地址预留的数量正确
    {ok, MainAccountAddr} = erthereum:eth_coinbase(),
    %{ok, } = erthereum:(MainAccountAddr),
    % 按合约里兑换比率和软顶，在第一阶段打入11000个eth，即结束软顶
    ok.

distribute_ETH(AccountList) ->
    % 每个账号划拨50000ETH
    {ok, MainAccountAddr} = erthereum:eth_coinbase(),
    [begin
         {ok, _} = erthereum:eth_sendTransaction(MainAccountAddr, Addr, {ether, 50000}),
         timer:sleep(1000)
     end || {_, Addr} <- AccountList],
    ok.

deploy_contract(AccountList) ->
    {ok, C0} = file:read_file("../../priv/bgx.sol"),
    ct:log("raw contract: ~ts~n", [C0]),
    % $START_TIME
    StartTime = util:unixtime() + 120, % 2分钟后
    StartTimeBin = integer_to_binary(StartTime),
    C1 = binary:replace(C0, <<"$START_TIME">>, StartTimeBin),
    % $ETH_FUND_ADDR
    [{_, EthFundAddr} | _] = AccountList,
    C2 = binary:replace(C1, <<"$ETH_FUND_ADDR">>, EthFundAddr),
    % $FOUNDATION_ADDRS，15个
    FoundationAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 2, 15)],
    FoundationAddrs = list_to_binary(util:implode(", ", FoundationAddrList)),
    C3 = binary:replace(C2, <<"$FOUNDATION_ADDRS">>, FoundationAddrs),
    % $TEAM_ADDRS，15个
    TeamAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 17, 15)],
    TeamAddrs = list_to_binary(util:implode(", ", TeamAddrList)),
    C4 = binary:replace(C3, <<"$TEAM_ADDRS">>, TeamAddrs),
    % $MINING_ADDRS，2个
    MiningAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 32, 2)],
    MiningAddrs = list_to_binary(util:implode(", ", MiningAddrList)),
    C5 = binary:replace(C4, <<"$MINING_ADDRS">>, MiningAddrs),
    % $CORNERSTONE_ADDRS，10个
    CornerstoneAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 34, 10)],
    CornerstoneAddrs = list_to_binary(util:implode(", ", CornerstoneAddrList)),
    C6 = binary:replace(C5, <<"$CORNERSTONE_ADDRS">>, CornerstoneAddrs),
    % $PREICO_ADDRS，5个
    PreIcoAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 44, 5)],
    PreIcoAddrs = list_to_binary(util:implode(", ", PreIcoAddrList)),
    C = binary:replace(C6, <<"$PREICO_ADDRS">>, PreIcoAddrs),
    ct:log("refilled contract: ~ts~n", [C]),
    % 编译合约
    %eth_compileSolidity are gone in go-ethereum 1.6.0
    %The method eth_compileSolidity does not exist/is not available
    %{ok, Contract} = erthereum:eth_compileSolidity(C),
    file:write_file("refilled_bgx.sol", C),
    os:cmd("solcjs -o . --bin --abi refilled_bgx.sol"),
    {ok, Contract} = file:read_file("refilled_bgx_sol_BGCToken.bin"),
    % 部署合约
    {ok, MainAccountAddr} = erthereum:eth_coinbase(),
    {ok, TransactionHash} = erthereum:eth_deployContract(MainAccountAddr, <<"0x", Contract/binary>>),
    ct:log("contract creating transaction hash: ~s~n", [TransactionHash]),
    timer:sleep(5000),
    {ok, {TransactionReceipt}} = erthereum:eth_getTransactionReceipt(TransactionHash),
    ct:log("contract info: ~p~n", [TransactionReceipt]),
    {_, ContractAddr} = lists:keyfind(<<"contractAddress">>, 1, TransactionReceipt),
    {ok, ContractAddr}.

