-module(gitto_config).
-export([example/0, get_value/2]).

example() ->
    %% Where cloned repositories are stored.
    [{bare_reps_dir, filename:join(code:lib_dir(gitto, data), bare_reps)}
    %% Where which interesting version of a cloned repository are stored.
    ,{rev_reps_dir,  filename:join(code:lib_dir(gitto, data), rev_reps)}
    %% Where automatically generated repositories are stored.
    ,{test_reps_dir, filename:join(code:lib_dir(gitto, data), test_reps)}
    ].


get_value(Value, Cfg) ->
    proplists:get_value(Value, Cfg).
