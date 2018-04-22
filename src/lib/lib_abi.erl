-module(lib_abi).
-compile(export_all).

encode_data(FunName, ParamTypes, ParamVals) ->
    EncodedFunSig = encode_funsig(FunName, ParamTypes),
    EncodedParams = << <<(encode_param(ParamType, ParamVal))/binary>> || {ParamType, ParamVal} <- lists:zip(ParamTypes, ParamVals) >>,
    <<EncodedFunSig/binary, EncodedParams/binary>>.

encode_funsig(FunName, ParamTypes) ->
    <<>>.

encode_param(<<"uint8">>, V) ->
    <<>>;
encode_param(<<"uint256">>, V) ->
    <<>>;
encode_param(<<"bool">>, V) ->
    <<>>;
encode_param(<<"string">>, V) ->
    <<>>;
encode_param(<<"bytes">>, V) ->
    <<>>;
encode_param(<<"address">>, V) ->
    <<>>;
encode_param(_, _) ->
    <<>>.

