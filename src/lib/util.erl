%%%-----------------------------------
%%% @Module  : util
%%% @Description: 公共函数
%%%-----------------------------------

-include("common.hrl").

-module(util).
-export([
        now/0,
        unixtime/0,
        longunixtime/0,
        rand/2,
        round2d/1,
        ceil/1,
        floor/1,
        implode/2,
        implode/3,
        explode/2,
        explode/3,
        string_to_term/1,
        bitstring_to_term/1,
        term_to_string/1,
        term_to_bitstring/1,
        term_to_bitstring_p/1,
        lookup_one/2,
        lookup_one/3,
        lookup_all/2,
        lookup_all/3,
        match_one/2,
        match_all/2,
        url_encode/1,
        url_decode/1,
        html_unescape/1,
        upper_1st_char/1,
        esc/1,
        traverse_ets/3
	  ]
).

now() ->
    os:timestamp().

%% 取得当前的unix时间戳（单位为秒）
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% 取得当前的unix时间戳（单位为毫秒）
longunixtime() ->
    {M, S, Ms} = os:timestamp(),
    M * 1000000000 + S*1000 + Ms div 1000.

%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) when Min > Max -> 0;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    case get(random_seed) of
        undefined ->
            <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
            RandSeed = {A, B, C},
            rand:seed(exsplus, RandSeed),
            put(random_seed, RandSeed);
        _ -> skip
    end,
    M = Min - 1,
    rand:uniform(Max - M) + M.

%%四舍五入保留两位小数
round2d(N) ->
    round(N*100)/100.

%%向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, []) ->
    [<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 以分隔符拆分字符串
explode(S, B) ->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    %%{ok, S} = file:open("string_to_term.txt", [append]),
    %%io:format(S, "~p~n", [String]),
    %%file:close(S),
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err ->
                    io:format("~nerl_parse wrong:==>~n~ts~n~p~n<==~n", [String, erlang:process_info(self(), current_stacktrace)]),
                    undefined
            end;
        _Error ->
            io:format("~nerl_scan wrong:==>~n~ts~n~p~n<==~n", [String, erlang:process_info(self(), current_stacktrace)]),
            undefined
    end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(<<>>) -> undefined;
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(<<$<, Char:8, _/binary>>) when Char =/= $< -> undefined; % 使pid无效
bitstring_to_term(BitString) ->
    case binary:match(BitString, [<<"#Ref<">>], []) of
        nomatch -> string_to_term(binary_to_list(BitString));
        _ -> undefined
    end.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
%%当列表中的值全小于255时，~p会显示字母，所以用~w，同时有个好处，对于字符串中含有单引号等字符时，不会导致sql语法错误
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

term_to_bitstring_p(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

lookup_one(Table, Key) ->
    lookup_one(Table, Key, false).

lookup_one(Table, Key, false) ->
    case ets:lookup(Table, Key) of
        [] -> [];
        [R] -> R
    end;
lookup_one(Table, Key, true) ->
    case mnesia:dirty_read(Table, Key) of
        [] -> [];
        [R] -> R
    end.

lookup_all(Table, Key) ->
    lookup_all(Table, Key, false).

lookup_all(Table, Key, false) ->
    ets:lookup(Table, Key);
lookup_all(Table, Key, true) ->
    mnesia:dirty_read(Table, Key).

match_one(Table, Pattern) ->
    case ets:match_object(Table, Pattern) of
        [] -> [];
        [R] -> R
    end.

match_all(Table, Pattern) ->
    ets:match_object(Table, Pattern).

%% url编码
url_encode(B) when is_list(B) ->
    binary_to_list(url_encode(list_to_binary(B), <<>>));
url_encode(B) when is_binary(B) ->
    url_encode(B, <<>>).

url_encode(<< $\s, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $+ >>);
url_encode(<< $-, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $- >>);
url_encode(<< $., Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $. >>);
url_encode(<< $0, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $0 >>);
url_encode(<< $1, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $1 >>);
url_encode(<< $2, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $2 >>);
url_encode(<< $3, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $3 >>);
url_encode(<< $4, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $4 >>);
url_encode(<< $5, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $5 >>);
url_encode(<< $6, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $6 >>);
url_encode(<< $7, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $7 >>);
url_encode(<< $8, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $8 >>);
url_encode(<< $9, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $9 >>);
url_encode(<< $A, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $A >>);
url_encode(<< $B, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $B >>);
url_encode(<< $C, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $C >>);
url_encode(<< $D, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $D >>);
url_encode(<< $E, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $E >>);
url_encode(<< $F, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $F >>);
url_encode(<< $G, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $G >>);
url_encode(<< $H, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $H >>);
url_encode(<< $I, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $I >>);
url_encode(<< $J, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $J >>);
url_encode(<< $K, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $K >>);
url_encode(<< $L, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $L >>);
url_encode(<< $M, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $M >>);
url_encode(<< $N, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $N >>);
url_encode(<< $O, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $O >>);
url_encode(<< $P, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $P >>);
url_encode(<< $Q, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $Q >>);
url_encode(<< $R, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $R >>);
url_encode(<< $S, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $S >>);
url_encode(<< $T, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $T >>);
url_encode(<< $U, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $U >>);
url_encode(<< $V, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $V >>);
url_encode(<< $W, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $W >>);
url_encode(<< $X, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $X >>);
url_encode(<< $Y, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $Y >>);
url_encode(<< $Z, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $Z >>);
url_encode(<< $_, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $_ >>);
url_encode(<< $a, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $a >>);
url_encode(<< $b, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $b >>);
url_encode(<< $c, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $c >>);
url_encode(<< $d, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $d >>);
url_encode(<< $e, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $e >>);
url_encode(<< $f, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $f >>);
url_encode(<< $g, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $g >>);
url_encode(<< $h, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $h >>);
url_encode(<< $i, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $i >>);
url_encode(<< $j, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $j >>);
url_encode(<< $k, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $k >>);
url_encode(<< $l, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $l >>);
url_encode(<< $m, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $m >>);
url_encode(<< $n, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $n >>);
url_encode(<< $o, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $o >>);
url_encode(<< $p, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $p >>);
url_encode(<< $q, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $q >>);
url_encode(<< $r, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $r >>);
url_encode(<< $s, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $s >>);
url_encode(<< $t, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $t >>);
url_encode(<< $u, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $u >>);
url_encode(<< $v, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $v >>);
url_encode(<< $w, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $w >>);
url_encode(<< $x, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $x >>);
url_encode(<< $y, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $y >>);
url_encode(<< $z, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $z >>);
url_encode(<< C, Rest/bits >>, Acc) ->
    H = hex(C bsr 4),
    L = hex(C band 16#0f),
    url_encode(Rest, << Acc/bits, $%, H, L >>);
url_encode(<<>>, Acc) ->
    Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.

%% url解码
url_decode(B) when is_list(B) ->
    binary_to_list(url_decode(list_to_binary(B), <<>>));
url_decode(B) when is_binary(B) ->
    url_decode(B, <<>>).

url_decode(<< $%, H, L, Rest/bits >>, Acc) ->
    C = (unhex(H) bsl 4 bor unhex(L)),
    url_decode(Rest, << Acc/bits, C >>);
url_decode(<< $+, Rest/bits >>, Acc) ->
    url_decode(Rest, << Acc/bits, " " >>);
url_decode(<< C, Rest/bits >>, Acc) when C =/= $% ->
    url_decode(Rest, << Acc/bits, C >>);
url_decode(<<>>, Acc) ->
    Acc.

unhex($0) ->  0;
unhex($1) ->  1;
unhex($2) ->  2;
unhex($3) ->  3;
unhex($4) ->  4;
unhex($5) ->  5;
unhex($6) ->  6;
unhex($7) ->  7;
unhex($8) ->  8;
unhex($9) ->  9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.

%% html反转义
html_unescape(L) -> html_unescape(L, []).
html_unescape([], Acc) -> lists:reverse(Acc);
html_unescape([$&,$l,$t,$;|T], Acc) -> html_unescape(T, [$<|Acc]);
html_unescape([$&,$g,$t,$;|T], Acc) -> html_unescape(T, [$>|Acc]);
html_unescape([$&,$q,$u,$o,$t,$;|T], Acc) -> html_unescape(T, [$"|Acc]);
html_unescape([H|T], Acc) -> html_unescape(T, [H|Acc]).

upper_1st_char(<<First:8, Rest/binary>>) when First >= $a, First =< $z ->
    <<(First+$A-$a):8, Rest/binary>>;
upper_1st_char(Str) ->
    Str.

esc(BinString) ->
    BinString2 = binary:replace(BinString, <<"'">>, <<"''">>, [global]),
    binary:replace(BinString2, <<"\\">>, <<"\\\\">>, [global]).

% 遍历ets表
traverse_ets(F, FilterF, Table) ->
    traverse_ets(F, FilterF, Table, ets:first(Table), 1).

traverse_ets(F, FilterF, Table, '$end_of_table', I) ->
    ok;
traverse_ets(F, FilterF, Table, Key, I) ->
    case ets:lookup(Table, Key) of
        [R] ->
            case is_function(FilterF) of
                true -> % 设了过滤函数
                    case FilterF(R) of
                        true -> % 不过滤掉
                            F(I, R),
                            traverse_ets(F, FilterF, Table, ets:next(Table, Key), I+1);
                        false -> % 过滤掉
                            traverse_ets(F, FilterF, Table, ets:next(Table, Key), I)
                    end;
                false -> % 没设过滤函数
                    F(I, R),
                    traverse_ets(F, FilterF, Table, ets:next(Table, Key), I+1)
                    end;
        [] ->
            traverse_ets(F, FilterF, Table, ets:next(Table, Key), I)
    end.

