-module(gitto_config).
-export([example/0, get_value/2]).

example() ->
    [{bare_reps_dir, filename:join(code:priv_dir(gitto), bare_reps)}
    ,{rev_reps_dir,  filename:join(code:priv_dir(gitto), rev_reps)}].


get_value(Value, Cfg) ->
    proplists:get_value(Value, Cfg).
