-module(mycmd).
-export([cmd/2, cmd/3]).

cmd(Cmd, Args) ->
    cmd(Cmd, Args, []).

cmd(Cmd, Args, Opts) ->
    Tag = make_ref(), 
    {Pid, Ref} = erlang:spawn_monitor(fun() ->
            Rv = cmd_sync(Cmd, Args, Opts),
            exit({Tag, Rv})
        end),
    receive
        {'DOWN', Ref, process, Pid, {Tag, Data}} -> Data;
        {'DOWN', Ref, process, Pid, Reason} -> exit(Reason)
    end.

cmd_sync(Cmd, Args, Opts) ->
    P = open_port({spawn_executable, os:find_executable(Cmd)}, 
           Opts ++ [binary, use_stdio, stream, eof, {args, Args}, exit_status]),
    cmd_receive(P, []).

cmd_receive(Port, Acc) ->
    receive
        {Port, {data, Data}} -> 
            cmd_receive(Port, [Data|Acc]);
        {Port,{exit_status,Status}} -> 
            error_logger:info_msg("Exit status ~p of the ~p port.", 
                                  [Status, Port]),
            cmd_receive(Port, Acc);
        {Port, eof} -> 
            {ok, lists:reverse(Acc)}
    end.
