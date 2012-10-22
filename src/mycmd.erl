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
           Opts ++ [binary, use_stdio, stream, eof, {args, Args}, exit_status, 
                    stderr_to_stdout]),
    error_logger:info_msg("Command ~p:~p opens the ~p port with ~n~p.", 
                          [Cmd, Args, P, Opts]),
    cmd_receive(P, [], 0).

cmd_receive(Port, Acc, OldExitStatus) ->
    receive
        {Port, {data, Data}} -> 
            cmd_receive(Port, [Data|Acc], OldExitStatus);
        {Port,{exit_status,Status}} -> 
            error_logger:info_msg("Exit status ~p of the ~p port.", 
                                  [Status, Port]),
            cmd_receive(Port, Acc, Status);
        {Port, eof} -> 
            Data = lists:reverse(Acc),
            case OldExitStatus of
                0 -> {ok, Data};
                _ -> 
                    error_logger:info_msg("Data from ~p port.~n~p", 
                                  [Port, Data]),
                    {error, {bad_exit_status, [{exit_status, OldExitStatus}
                                              ,{data, Data}]}}
            end;
        Wtf ->
            error_logger:info_msg("Wtf recieved: ~p.", 
                                  [Wtf])

    end.
