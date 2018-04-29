-module(buy_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

% 1ETH
-define(WEI_PER_ETH, 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10).
% 1BGX（18位小数精度）
-define(UNIT_PER_BGX, 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10).
% 1亿BGX，以最小单位表示
-define(YI, 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * ?UNIT_PER_BGX).

all() ->
    [{group, buy}].

groups() ->
    [{buy, [parallel], [test_case_normal]}].

init_per_suite(Config) ->
    % 启动test
    bt:test_start(),
    Config.

end_per_suite(_Config) ->
    % 停止test
    bt:test_stop(),
    ok.

init_per_group(buy, Config) ->
    % 启动geth
    ct:log("starting geth...", []),
    % 先停止geth
    os:cmd("ps -efww|grep \"geth\"|grep -v grep|grep -v attach|tr -s ' '|cut -d ' ' -f 3|xargs kill -9"),
    % 如果要区块高度一直增高，启动geth时加入--dev.period 1（但会白耗CPU），否则只靠交易驱动挖矿
    spawn(fun() -> os:cmd("mkdir eth; geth --verbosity 5 --datadir ./eth/ --rpc --rpcapi eth,net,web3,personal --rpcport 8545 --dev --mine --minerthreads 1 --ipcpath /Users/guli1/Library/Ethereum/geth.ipc 2>>./eth/eth.log") end),
    ct:log("waiting 5 seconds...", []),
    timer:sleep(5000),
    % 创建50个账号
    ct:log("creating 50 accounts...", []),
    L = [begin
             {ok, Addr} = erthereum:personal_newAccount(<<"test">>),
             ct:log("account ~p created: ~p~n", [I, Addr]),
             {I, Addr}
         end || I <- lists:seq(1, 50)],
    % 发布智能合约（用掉近500万gas）
    ct:log("deploying contract...", []),
    {ok, ContractAddr} = deploy_contract(L),
    % 主账号向50个账号划拨ETH
    distribute_ETH(L),
    [{accounts, L}, {contract_addr, ContractAddr} | Config];
init_per_group(_Name, Config) ->
    Config.

end_per_group(_Name, _Config) ->
    % 停止geth（放在初始化）
    ok.

test_case_normal(Config) ->
    {ok, MainAccountAddr} = erthereum:eth_coinbase(),
    {_, ContractAddr} = lists:keyfind(contract_addr, 1, Config),
    % 后29个账号可给合约地址打入ETH
    {_, L} = lists:keyfind(accounts, 1, Config),
    [{_, EthFundAddr}|_] = L,
    [_, {_, IcoAddr}|_] = L,
    [{_, HeadAddr}|_] = AccountList = lists:sublist(L, 22, 29),
    % 打入0.09是不成功的（最少0.1）
    {ok, EthFundAddrBalanceOld} = erthereum:eth_getBalance(EthFundAddr),
    {ok, true} = erthereum:personal_unlockAccount(HeadAddr, <<"test">>),
    {ok, _} = erthereum:eth_sendTransaction(HeadAddr, ContractAddr, {ether, 0.09}),
    timer:sleep(1000),
    {ok, EthFundAddrBalanceOld} = erthereum:eth_getBalance(EthFundAddr),
    % 众账号随机打入30000个ETH，达到硬顶（分3轮，每轮10000ETH）
    BuyF = fun() ->
               RandL = [util:rand(1, 1000) || _I <- lists:seq(1, 29)],
               RandS = lists:sum(RandL),
               [H|T] = BuyL0 = [0.1 + util:round2d((Rand / RandS) * (10000 - 0.1 * 29))  || Rand <- RandL],
               BuyS = lists:sum(BuyL0),
               Remainder = 10000 - BuyS,
               BuyL = [util:ceil(H+Remainder)|T],
               [begin
                    {ok, true} = erthereum:personal_unlockAccount(Addr, <<"test">>),
                    {ok, TransactionHash} = erthereum:eth_sendTransaction(Addr, ContractAddr, {ether, Amount}),
                    timer:sleep(1000),
                    {ok, {TransactionReceipt}} = erthereum:eth_getTransactionReceipt(TransactionHash),
                    ct:log("transaction info: ~p~n", [TransactionReceipt])
                end || {{_, Addr}, Amount} <- lists:zip(AccountList, BuyL)]
           end,
    BuyF(),
    BuyF(),
    BuyF(),
    {ok, {wei,30000 * ?WEI_PER_ETH}} = erthereum:eth_getBalance(EthFundAddr),
    % 白名单地址在结束前，可以转BGX给其他地址
    OneWhiteAddr = IcoAddr,
    OneOtherAddr = HeadAddr,
    {ok, true} = erthereum:personal_unlockAccount(OneWhiteAddr, <<"test">>),
    {ok, true} = erthereum:personal_unlockAccount(OneOtherAddr, <<"test">>),
    {ok, _} = call_contract:transfer(OneWhiteAddr, ContractAddr, OneOtherAddr, integer_to_binary(1000 * ?UNIT_PER_BGX)),
    timer:sleep(3000),
    % 因为目前合约逻辑里只管收钱，并未将相应的BGX转进打入ETH的地址，所以这里余额应该等于1000
    {ok, OneOtherBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, OneOtherAddr),
    OneOtherBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(1000 * ?UNIT_PER_BGX)),
    % 非白名单地址在结束前，不可转账，直到锁定结束后
    {ok, _} = call_contract:transfer(OneOtherAddr, ContractAddr, OneWhiteAddr, integer_to_binary(1000 * ?UNIT_PER_BGX)),
    timer:sleep(3000),
    % 不能转出，所以余额还是1000
    {ok, OneOtherBalance2} = call_contract:balanceOf(MainAccountAddr, ContractAddr, OneOtherAddr),
    OneOtherBalance2 = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(1000 * ?UNIT_PER_BGX)),
    % 手动设置lockEndTime结束
    Now = util:unixtime() - 30,
    {ok, _} = call_contract:setLockEndTime(MainAccountAddr, ContractAddr, integer_to_binary(Now)),
    timer:sleep(3000),
    {ok, _} = call_contract:transfer(OneOtherAddr, ContractAddr, OneWhiteAddr, integer_to_binary(1000 * ?UNIT_PER_BGX)),
    timer:sleep(3000),
    % 能转出，所以余额是0
    {ok, OneOtherBalance3} = call_contract:balanceOf(MainAccountAddr, ContractAddr, OneOtherAddr),
    OneOtherBalance3 = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(0)),
    ok.

distribute_ETH(AccountList0) ->
    % 后49个账号，每个账号划拨50000ETH（第1个地址ICO接收ETH，避免干扰，不给它划拨ETH）
    [_|AccountList] = AccountList0,
    {ok, MainAccountAddr} = erthereum:eth_coinbase(),
    [begin
         {ok, _} = erthereum:eth_sendTransaction(MainAccountAddr, Addr, {ether, 50000}),
         timer:sleep(1000)
     end || {_I, Addr} <- AccountList],
    ok.

deploy_contract(AccountList) ->
    {ok, C0} = file:read_file("../../priv/bgx.sol"),
    ct:log("raw contract: ~ts~n", [C0]),
    % $START_TIME
    StartTime = util:unixtime(), % 立即开始
    StartTimeBin = integer_to_binary(StartTime),
    C1 = binary:replace(C0, <<"$START_TIME">>, StartTimeBin),
    % $END_TIME
    EndTime = StartTime + 3 * 3600, % 3小时后
    EndTimeBin = integer_to_binary(EndTime),
    C2 = binary:replace(C1, <<"$END_TIME">>, EndTimeBin),
    % $LOCK_END_TIME
    LockEndTime = EndTime,
    LockEndTimeBin = integer_to_binary(LockEndTime),
    C3 = binary:replace(C2, <<"$LOCK_END_TIME">>, LockEndTimeBin),
    % $ETH_FUND_ADDR
    [{_, EthFundAddr} | _] = AccountList,
    C4 = binary:replace(C3, <<"$ETH_FUND_ADDR">>, EthFundAddr),
    % $ICO_ADDR
    [_, {_, IcoAddr} | _] = AccountList,
    C5 = binary:replace(C4, <<"$ICO_ADDR">>, IcoAddr),
    % $FOUNDATION_ADDRS，7个
    FoundationAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 3, 7)],
    FoundationAddrs = list_to_binary(util:implode(", ", FoundationAddrList)),
    C6 = binary:replace(C5, <<"$FOUNDATION_ADDRS">>, FoundationAddrs),
    % $TEAM_ADDRS，1个
    TeamAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 10, 1)],
    TeamAddrs = list_to_binary(util:implode(", ", TeamAddrList)),
    C7 = binary:replace(C6, <<"$TEAM_ADDRS">>, TeamAddrs),
    % $MINING_ADDRS，2个
    MiningAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 11, 2)],
    MiningAddrs = list_to_binary(util:implode(", ", MiningAddrList)),
    C8 = binary:replace(C7, <<"$MINING_ADDRS">>, MiningAddrs),
    % $ANGEL_ADDRS，1个
    AngelAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 13, 1)],
    AngelAddrs = list_to_binary(util:implode(", ", AngelAddrList)),
    C9 = binary:replace(C8, <<"$ANGEL_ADDRS">>, AngelAddrs),
    % $CORNERSTONE_ADDRS，7个
    CornerstoneAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 14, 7)],
    CornerstoneAddrs = list_to_binary(util:implode(", ", CornerstoneAddrList)),
    C10 = binary:replace(C9, <<"$CORNERSTONE_ADDRS">>, CornerstoneAddrs),
    % $PREICO_ADDRS，1个
    PreIcoAddrList = [Addr || {_, Addr} <- lists:sublist(AccountList, 21, 1)],
    PreIcoAddrs = list_to_binary(util:implode(", ", PreIcoAddrList)),
    C = binary:replace(C10, <<"$PREICO_ADDRS">>, PreIcoAddrs),
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
    abi_codegen:parse_abi_file("refilled_bgx_sol_BGCToken.abi"),
    % 先检查各地址预留的BGX数量正确
    % ICO账户
    {ok, IcoAddrBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, IcoAddr),
    IcoAddrBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(20 * ?YI)),
    % 基金账户
    [begin
         {ok, AddrBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, Addr),
         AddrBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(YiAmount * ?YI))
     end || {Addr, YiAmount} <- lists:zip(FoundationAddrList, [5, 5, 1, 1, 1, 1, 1])],
    % 团队账户
    [begin
         {ok, AddrBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, Addr),
         AddrBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(YiAmount * ?YI))
     end || {Addr, YiAmount} <- lists:zip(TeamAddrList, [15])],
    % 挖矿账户
    [begin
         {ok, AddrBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, Addr),
         AddrBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(YiAmount * ?YI))
     end || {Addr, YiAmount} <- lists:zip(MiningAddrList, [15, 15])],
    % 天使账户
    [begin
         {ok, AddrBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, Addr),
         AddrBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(YiAmount * ?YI))
     end || {Addr, YiAmount} <- lists:zip(AngelAddrList, [5])],
    % 基石账户
    [begin
         {ok, AddrBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, Addr),
         AddrBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(YiAmount * ?YI))
     end || {Addr, YiAmount} <- lists:zip(CornerstoneAddrList, [1, 1, 1, 1, 1, 2, 3])],
    % PreICO账户
    [begin
         {ok, AddrBalance} = call_contract:balanceOf(MainAccountAddr, ContractAddr, Addr),
         AddrBalance = lib_abi:encode_param_with_0x(<<"uint256">>, integer_to_binary(YiAmount * ?YI))
     end || {Addr, YiAmount} <- lists:zip(PreIcoAddrList, [5])],
    {ok, ContractAddr}.

