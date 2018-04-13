%%%------------------------------------------------
%%% File    : common.hrl
%%% Description: 公共定义
%%%------------------------------------------------

% use lager log system: debug, info, notice, warning, error, critical, alert, emergency
-ifdef(debug).
    -define(DEBUG(F, A), lager:debug(F, A)).
    -define(DBG(Str, Args), lager:info(Str, Args)).
    -define(DBGS(Str), lager:info(Str)).
-else.
    -define(DEBUG(F, A), ok).
    -define(DBG(Str, Args), ok).
    -define(DBGS(Str), ok).
-endif.
-define(INFO(F, A), lager:info(F, A)).
-define(ERR(F, A), lager:error(F, A)).

