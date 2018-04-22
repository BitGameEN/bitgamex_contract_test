-module(abi_codegen).
-export([parse_abi_file/1]).

parse_abi_file(File) ->
    {ok, AbiJson} = file:read_file(File),
    AbiList = jiffy:decode(AbiJson),
    ModuleName = <<"call_contract">>,
    ModuleFile = "./call_contract.erl",
    {ok, S} = file:open(ModuleFile, write),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "%%% @Module: ~s~n", [ModuleName]),
    io:format(S, <<"%%% @Description: 自动生成~n"/utf8>>, []),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "-module(~s).~n", [ModuleName]),
    io:format(S, "-compile(export_all).~n~n", []),
    [gen_abi(S, One) || One <- AbiList],
    {ok, Module} = c:c(ModuleFile),
    {module, _} = c:l(Module),
    ok.

gen_abi(S, {Abi}) ->
    case lists:keyfind(<<"type">>, 1, Abi) of
        {_, <<"function">>} ->
            {_, FunName} = lists:keyfind(<<"name">>, 1, Abi),
            {_, Inputs} = lists:keyfind(<<"inputs">>, 1, Abi),
            ParamNameTypePairs =
                [begin
                    {_, ParamName0} = lists:keyfind(<<"name">>, 1, One),
                    {_, ParamType} = lists:keyfind(<<"type">>, 1, One),
                    ParamName =
                        case ParamName0 of
                            <<>> -> ParamType;
                            _ -> ParamName0
                        end,
                    {ParamName, ParamType}
                 end || {One} <- Inputs],
            {ParamNames0, ParamTypes} = lists:unzip(ParamNameTypePairs),
            ParamNames = [util:upper_1st_char(list_to_binary(string:strip(binary_to_list(ParamName), both, $_))) || ParamName <- ParamNames0],
            ConcatParamNames = concat(ParamNames, ", "),
            ConcatParamTypes = concat(ParamTypes, ", "),
            io:format(S, "~s(FromAddress, ContractAddress~s) ->~n", [FunName, case ConcatParamNames of <<>> -> <<>>; _ -> <<", ", ConcatParamNames/binary>> end]),
            {_, IsConstant} = lists:keyfind(<<"constant">>, 1, Abi),
            io:format(S, "\terthereum:eth_callContract(FromAddress, ContractAddress, lib_abi:encode_data(~s, [~s], [~s]), ~p).~n~n", [FunName, ConcatParamTypes, ConcatParamNames, IsConstant]),
            ok;
        _ -> % constructor、fallback、event
            void
    end.

% <<"x, y">>
concat([], _) ->
    <<>>;
concat([Name], _) ->
    Name;
concat(Names, Imploder) ->
    L = util:implode(Imploder, Names),
    list_to_binary(L).

