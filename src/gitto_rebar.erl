-module(gitto_rebar).
-export([compile/1]).

compile(RepDir) ->
    cmd("rebar", ["compile", "skip_deps=true"], [{cd, RepDir}]).

cmd(Cmd, Args, Opts) ->
    case mycmd:cmd(Cmd, Args, Opts) of
        {ok, Data} -> Data;
        {error, Reason} -> erlang:error(Reason)
    end.

