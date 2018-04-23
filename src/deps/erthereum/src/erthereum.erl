-module(erthereum).

%% API exports
%% net namespace
-export([net_version/0]).
%% eth namespace
-export([eth_blockNumber/0,
         eth_getBalance/1, eth_getBalance/2,
         eth_accounts/0,
         eth_coinbase/0,
         eth_compileSolidity/1,
         eth_deployContract/2,
         eth_callContract/4,
         eth_sendTransaction/3,
         eth_getTransactionReceipt/1]).
%% personal namespace
-export([personal_newAccount/1,
         personal_unlockAccount/2]).
%% web3 namespace
-export([web3_sha3/1]).

-export([convert/1]).

-include("common.hrl").

%% Type definitions.
-type error() :: failed | decoding_failed.
-type management_api_method() :: net_version |
                                 eth_blockNumber |
                                 eth_getBalance |
                                 eth_accounts |
                                 eth_sendTransaction |
                                 personal_newAccount |
                                 personal_unlockAccount.
-type method_name() :: binary().
-type method_id() :: non_neg_integer().
-type address() :: binary().
-type passphrase() :: binary().
-type block_tag() :: latest | earliest | pending.
-type quantity() :: integer().
-type data() :: binary().
-type wei() :: {wei, integer()}.
-type ether() :: {ether, number()}.

-type json_value() :: null
                    | true
                    | false
                    | json_string()
                    | json_number()
                    | json_object()
                    | json_array().

-type json_array()  :: [json_value()].
-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().

-type json_object() :: {[{json_string(),json_value()}]}.

%% Type exports
-export_type([error/0]).

%% Defines
-define(ETHER_WEI_EXACHANGE_RATE, 1000000000000000000).

%%====================================================================
%% API functions
%%====================================================================

%% Returns the current network protocol version.
-spec net_version() -> {ok, non_neg_integer()} | {error, error()}.
net_version() ->
    maybe_int(request(net_version)).

%% Returns the number of most recent block.
-spec eth_blockNumber() -> {ok, non_neg_integer()} | {error, error()}.
eth_blockNumber() ->
    maybe_int(request(eth_blockNumber)).

%% Returns the balance of the account of given address.
-spec eth_getBalance(Address :: address()) -> {ok, wei()} | {error, error()}.
eth_getBalance(Address) ->
    eth_getBalance(Address, latest).

%% Returns the balance of the account of given address.
-spec eth_getBalance(Address :: address(),
                     BlockNumberOrTag :: non_neg_integer() | block_tag())
        -> {ok, wei()} | {error, error()}.
eth_getBalance(Address, BlockNumberOrTag0) ->
    BlockNumberOrTag = block_number_or_tag(BlockNumberOrTag0),
    maybe_wei(request(eth_getBalance, [Address, BlockNumberOrTag])).

%% Returns a list of addresses owned by client.
-spec eth_accounts() -> {ok, list(address())} | {error, error()}.
eth_accounts() ->
    maybe_list(request(eth_accounts)).

%% Returns the client coinbase address.
-spec eth_coinbase() -> {ok, address()} | {error, error()}.
eth_coinbase() ->
    request(eth_coinbase).

%% Returns compiled solidity code.
-spec eth_compileSolidity(SourceCode :: binary()) -> {ok, binary()} | {error, error()}.
eth_compileSolidity(SourceCode) ->
    request(eth_compileSolidity, [SourceCode]).

-spec eth_deployContract(FromAddress :: address(),
                         Data :: data()) -> {ok, TransactionHash :: data()} | {error, error()}.
eth_deployContract(FromAddress, Data) ->
    Params = [{[
                   {<<"from">>, FromAddress},
                   {<<"data">>, Data},
                   {<<"value">>, <<"0x0">>},
                   {<<"gasPrice">>, <<"0x174876e800">>},
                   {<<"gas">>, <<"0x5b8d80">>} % 要>53000，同时要小于当前块的gasLimit，并且保证足够多，否则耗尽gas导致合约不能创建成功
               ]}],
    maybe_binary(request(eth_sendTransaction, Params)).

-spec eth_callContract(FromAddress :: address(),
                       ContractAddress :: address(),
                       Data :: data(),
                       IsLocal ::boolean()) -> {ok, TransactionHash :: data()} | {error, error()}.
eth_callContract(FromAddress, ContractAddress, Data, IsLocal) ->
    ?DBG("FromAddress:~s~nContractAddress:~s~nData:~s~nIsLocal:~p~n", [FromAddress, ContractAddress, Data, IsLocal]),
    Params = [{[
                   {<<"from">>, FromAddress},
                   {<<"to">>, ContractAddress},
                   {<<"data">>, Data}
               ]}],
    Method = case IsLocal of
                 true -> eth_call;
                 false -> eth_sendTransaction
             end,
    maybe_binary(request(Method, case IsLocal of true -> Params ++ [<<"latest">>]; false -> Params end)).

%% Creates new message call transaction or a contract creation, if the data field contains code
-spec eth_sendTransaction(FromAddress :: address(),
                          ToAddress :: address(),
                          Value :: quantity() | wei() | ether()) -> {ok, TransactionHash :: data()} | {error, error()}.
eth_sendTransaction(FromAddress, ToAddress, Value0) ->
    Value = to_wei(Value0),
    Params = [{[
                   {<<"from">>, FromAddress},
                   {<<"to">>, ToAddress},
                   {<<"value">>, eth_int(Value)}
               ]}],
    maybe_binary(request(eth_sendTransaction, Params)).

-spec eth_getTransactionReceipt(TransactionHash :: data()) -> {ok, TransactionReceipt :: json_object()} | {error, error()}.
eth_getTransactionReceipt(TransactionHash) ->
    maybe_object(request(eth_getTransactionReceipt, [TransactionHash])).

%% Generates a new private key and stores it in the key storese directory.
%% The key file is encrypted with the given passphrase.
%% Returns the address of the new account.
-spec personal_newAccount(Passphrase :: passphrase())
        -> {ok, address()} | {error, error()}.
personal_newAccount(Passphrase) ->
    maybe_binary(request(personal_newAccount, [Passphrase])).

-spec personal_unlockAccount(Address :: address(),
                             Passphrase :: passphrase()
                            )
                            -> {ok, address()} | {error, error()}.
personal_unlockAccount(Address, Passphrase) ->
    maybe_boolean(request(personal_unlockAccount, [Address, Passphrase])).

%% Returns Keccak-256 (not the standardized SHA3-256) of the given data.
-spec web3_sha3(Data :: data()) -> {ok, data()} | {error, error()}.
web3_sha3(Data) ->
    request(web3_sha3, [Data]).

-spec convert(Value :: wei() | ether()) -> wei() | ether().
convert({ether, Value}) -> {wei, erlang:trunc(Value * ?ETHER_WEI_EXACHANGE_RATE)};
convert({wei, Value}) -> {ether, Value / ?ETHER_WEI_EXACHANGE_RATE}.

%%====================================================================
%% Internal functions
%%====================================================================
-spec request(Method :: management_api_method()) -> any() | {error, any()}.
request(Method) ->
    request(Method, []).

-spec request(Method :: management_api_method(),
              Arguments :: list()) -> any() | {error, any()}.
request(Method, Arguments) ->
    erth_request:request(management_api_data(Method), Arguments).

-spec block_number_or_tag(BlockNumberOrTag :: non_neg_integer() | block_tag()) -> binary().
block_number_or_tag(latest) -> <<"latest">>;
block_number_or_tag(earliest) -> <<"earliest">>;
block_number_or_tag(pending) -> <<"pending">>;
block_number_or_tag(BlockNumber) when is_integer(BlockNumber) -> eth_int(BlockNumber).

-spec maybe_int({ok, binary()} | error()) -> {ok, integer()} | error().
maybe_int({error, _} = Error) -> Error;
maybe_int({ok, <<"0x", Bin/binary>>}) ->
    maybe_int({ok, Bin});
maybe_int({ok, Bin}) ->
    case catch binary_to_integer(Bin) of
        I when is_integer(I) -> {ok, I};
        _ ->
            {ok, binary_to_integer(Bin, 16)}
    end.

-spec maybe_wei({ok, binary()} | error()) -> {ok, {wei, integer()}} | error().
maybe_wei({error, _} = Error) -> Error;
maybe_wei(Int) ->
    {ok, Wei} = maybe_int(Int),
    {ok, {wei, Wei}}.

-spec maybe_binary({ok, binary()} | error()) -> {ok, binary()} | error().
maybe_binary({error, _} = Error) -> Error;
maybe_binary({ok, Bin}) when is_binary(Bin) ->
    {ok, Bin}.

-spec maybe_list({ok, list(binary())} | error()) -> {ok, list(binary())} | error().
maybe_list({error, _} = Error) -> Error;
maybe_list({ok, L}) when is_list(L) ->
    {ok, L}.

-spec maybe_object({ok, json_object()} | error()) -> {ok, json_object()} | error().
maybe_object({error, _} = Error) -> Error;
maybe_object({ok, {[_|_]} = Dic}) ->
    {ok, Dic}.

-spec maybe_boolean({ok, boolean()} | error()) -> {ok, boolean()} | error().
maybe_boolean({error, _} = Error) -> Error;
maybe_boolean({ok, B}) when is_boolean(B) -> {ok, B}.

-spec eth_int(Integer :: integer()) -> binary().
eth_int(Integer) when is_integer(Integer) ->
    <<"0x", (integer_to_binary(Integer, 16))/binary>>.

-spec to_wei(Value :: wei() | ether() | non_neg_integer()) -> non_neg_integer().
to_wei({ether, _} = Ether)           -> to_wei(convert(Ether));
to_wei({wei, Wei})                   -> Wei;
to_wei(Value) when is_integer(Value) -> Value.

%% https://github.com/ethereum/wiki/wiki/JSON-RPC
-spec management_api_data(Method :: management_api_method())
        -> {method_name(), method_id()} | method_name().
management_api_data(net_version) ->
    {<<"net_version">>, 1};
management_api_data(eth_blockNumber) ->
    {<<"eth_blockNumber">>, 83};
management_api_data(eth_getBalance) ->
    {<<"eth_getBalance">>, 1};
management_api_data(personal_newAccount) ->
    <<"personal_newAccount">>;
management_api_data(personal_unlockAccount) ->
    <<"personal_unlockAccount">>;
management_api_data(eth_coinbase) ->
    {<<"eth_coinbase">>, 64};
management_api_data(eth_accounts) ->
    {<<"eth_accounts">>, 1};
management_api_data(eth_compileSolidity) ->
    {<<"eth_compileSolidity">>, 1};
management_api_data(eth_call) ->
    {<<"eth_call">>, 1};
management_api_data(eth_sendTransaction) ->
    {<<"eth_sendTransaction">>, 1};
management_api_data(eth_getTransactionReceipt) ->
    {<<"eth_getTransactionReceipt">>, 1};
management_api_data(web3_sha3) ->
    {<<"web3_sha3">>, 64}.

