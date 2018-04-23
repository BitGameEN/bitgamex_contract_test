-module(lib_abi).
-compile(export_all).

encode_data(FunName0, ParamTypes0, ParamVals) ->
    FunName = list_to_binary(atom_to_list(FunName0)),
    ParamTypes = [list_to_binary(atom_to_list(ParamType)) || ParamType <- ParamTypes0],
    EncodedFunSig = encode_funsig(FunName, ParamTypes),
    EncodedParams = << <<(encode_param(ParamType, ParamVal))/binary>> || {ParamType, ParamVal} <- lists:zip(ParamTypes, ParamVals) >>,
    <<EncodedFunSig/binary, EncodedParams/binary>>.

encode_funsig(FunName, ParamTypes) ->
    % 使用web3_sha3
    ConcatParamTypes = concat(ParamTypes, ","),
    FunSig0 = <<FunName/binary, "(", ConcatParamTypes/binary, ")">>,
    FunSig = list_to_binary("0x" ++ lists:concat([integer_to_list(C, 16) || C <- binary_to_list(FunSig0)])),
    {ok, Sha3Val} = erthereum:web3_sha3(FunSig),
    binary:part(Sha3Val, 0, 10).

% 暂时只支持这3种，我们合约里传参用到的类型
encode_param(<<"address">>, V) -> % uint160
    42 = byte_size(V),
    list_to_binary(string:right(string:to_lower(binary_to_list(binary:part(V, 2, 40))), 64, $0));
encode_param(<<"uint256">>, V) ->
    list_to_binary(string:right(integer_to_list(binary_to_integer(V), 16), 64, $0));
encode_param(<<"bytes">>, V) ->
    % todo: 对于变长类型，先预留32字节给起始地址偏移，然后编码后续字段，等所有字段处理完后再按顺序处理每个变长字段，首先返填地址偏移，再填入字节长度，再填入内容数据
    <<>>;
encode_param(_, _) ->
    <<>>.

% <<"x,y">>
concat([], _) ->
    <<>>;
concat([Name], _) ->
    Name;
concat(Names, Imploder) ->
    L = util:implode(Imploder, Names),
    list_to_binary(L).

