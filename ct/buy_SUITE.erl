-module(buy_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [{group, buy}].

groups() ->
    [{buy, [parallel], ct_helper:all(?MODULE)}].

init_per_group(buy, Config) ->
    % 初始化操作
    Config;
init_per_group(_Name, Config) ->
    Config.

end_per_group(_Name, _Config) ->
    ok.

test_case1(Config) ->
    ok.