-module(gitto_rep).
-compile(export_all).

%% Clone from `RepURL' to `RepURL'.
clone(RepURL, RepPath) ->
    mycmd:cmd("git", ["clone", "--bare", RepURL, RepPath]).


checkout(BareRepDir, TargetRepDir, Version) -> 
    mycmd:cmd("git", ["clone", "--shared", BareRepDir, TargetRepDir]),
    mycmd:cmd("git", ["chechout", Version],
              [{cd, TargetRepDir}]).


log(RepDir) ->
    {ok, Data} = mycmd:cmd("git", ["log", "--pretty=medium", "--date=raw"],
              [{cd, RepDir}]),
    iolist_to_binary(Data).

    
config() ->
    [{bare_reps_dir, filename:join(code:priv_dir(gitto), bare_reps)}
    ,{rev_reps_dir,  filename:join(code:priv_dir(gitto), rev_reps)}].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

log_test() ->
    io:format(user, "~p~n", [log(code:lib_dir(gitto))]).


-endif.

