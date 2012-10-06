%%% @doc Bindings for the console git command.
%%% Low-level operations.
%%% This module know nothing about the database.
-module(gitto_rep).
-compile(export_all).


init(RepDir) ->
    mycmd:cmd("git", ["init"],  [{cd, RepDir}]).

add_all(RepDir) ->
    mycmd:cmd("git", ["add", "."],  [{cd, RepDir}]).

commit(RepDir, Comment) ->
    mycmd:cmd("git", ["commit", "-m", Comment],  [{cd, RepDir}]).

tag(RepDir, Comment) ->
    mycmd:cmd("git", ["tag", Comment],  [{cd, RepDir}]).

merge(RepDir, Version) ->
    mycmd:cmd("git", ["merge", Version],  [{cd, RepDir}]).

%% Clone from `RepURL' to `RepURL'.
%% git clone --bare
bare_clone(RepURL, RepDir) ->
    mycmd:cmd("git", ["clone", "--bare", RepURL, RepDir]).

clone(RepURL, RepDir) ->
    mycmd:cmd("git", ["clone", RepURL, RepDir]).

%% @doc Pull from `RepURL' to `RepDir'.
pull(RepURL, RepDir) ->
    mycmd:cmd("git", ["pull", RepURL],  [{cd, RepDir}]).

%% git clone --shared
checkout(BareRepDir, TargetRepDir, Revision) -> 
    mycmd:cmd("git", ["clone", "--shared", BareRepDir, TargetRepDir]),
    mycmd:cmd("git", ["chechout", Revision],
              [{cd, TargetRepDir}]).


%% git log --pretty=medium --date=raw
log(RepDir) ->
    Data = cmd("git", ["log", "--format=" ++ gitto_log:format(), "--date=raw"], 
               [{cd, RepDir}]),
    iolist_to_binary(Data).


whenchanged(RepDir, FileName) ->
    whenchanged(RepDir, FileName, []).

%% @doc Return undecoded list of revision numbers, when the file was modified.
%% git whatchanged --format="%H" rebar.config
whenchanged(RepDir, FileName, Flags) ->
    Data = cmd("git", 
               ["whatchanged", "--format=%H"] ++ Flags ++ [FileName], 
               [{cd, RepDir}]),
    iolist_to_binary(Data).

%% gitto_whenchanged(Str)


%% @doc Extract the context of the single file from a specific revision.
%% git show 650c7db44349a18eb0794af33cc30837c0d1c536:rebar.config
read_file(RepDir, Revision, FileName) ->
    Data = cmd("git", ["show", Revision ++ ":" ++ FileName], [{cd, RepDir}]),
    iolist_to_binary(Data).
    


cmd(Cmd, Args, Opts) ->
    case mycmd:cmd(Cmd, Args, Opts) of
        {ok, Data} -> Data;
        {error, Reason} -> erlang:error(Reason)
    end.

cmd(Cmd, Args) ->
    case mycmd:cmd(Cmd, Args) of
        {ok, Data} -> Data;
        {error, Reason} -> erlang:error(Reason)
    end.


%% The oldes version is last.
versions(RepDir, FN, Flags) ->
    RevList = gitto_whenchanged:parse_commits(whenchanged(RepDir, FN, Flags)),
    [{RevNum, 
      try read_file(RepDir, binary_to_list(RevNum), FN)
      catch error:{bad_exit_status, _} -> <<"">>
      end} || RevNum <- RevList].

rebar_versions(RepDir, Flags) ->
    Configs = versions(RepDir, "rebar.config", Flags),
    [{RevNum, proplists:get_value(deps, 
                                  gitto_utils:consult_string(Data))}
        || {RevNum, Data} <- Configs].



-record(dep, {from, to, value}).

%% @doc This function gets a set of commits only from one chain of commits.
%% It means, that you usually want to use it with: `git log --first-parent'.
dependency_durations(Deps) ->
    dependency_durations(Deps, [], [], []).

dependency_durations([{RevNum, NewDeps}|T], OldDeps, From, To) ->
    {Deleted, _Same, Added} = update_dependencies(OldDeps, NewDeps),
    From2 = merge(RevNum, Added, From),
    To2   = merge(RevNum, Deleted, To),
    dependency_durations(T, NewDeps, From2, To2);

dependency_durations([], OldDeps, From, To) ->
    To2   = merge(latest, OldDeps, To),
    To3   = lists:keysort(2, To2),
    From2 = lists:keysort(2, From),
    Z = fun({F, Deps}, {T, Deps}) -> 
            #dep{from = F, to = T, value = Deps} 
        end,
    lists:zipwith(Z, From2, To3).
    

merge(RevNum, [H|T], Acc) ->
    merge(RevNum, T, [{RevNum, H} | Acc]);

merge(_RevNum, [], Acc) ->
    Acc.


-spec update_dependencies(OldDeps, NewDeps) -> {Deleted, Same, Added} when
    OldDeps :: Deps,
    NewDeps :: Deps,
    Deleted :: Deps,
    Same    :: Deps,
    Added   :: Deps,
    Deps    :: [term()].

update_dependencies(OldDeps, NewDeps) ->
    Deleted = OldDeps -- NewDeps,
    Same    = OldDeps -- Deleted,
    Added   = NewDeps -- OldDeps,
    {Deleted, Same, Added}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

log_test() ->
    RepDir = code:lib_dir(gitto),
    Data = log(RepDir),
    io:format(user, "~p~n", [Data]),
    Commits = gitto_log:parse_commits(Data),
    io:format(user, "~p~n", [Commits]).


read_file_test() ->
    RepDir = code:lib_dir(gitto),
    Data = read_file(RepDir, 
                     "650c7db44349a18eb0794af33cc30837c0d1c536",
                     "rebar.config"),
    io:format(user, "~p~n", [Data]).

read_file_from_wrong_revision_test() ->
    RepDir = code:lib_dir(gitto),
    ?assertError({bad_exit_status, _},
                 read_file(RepDir, 
                           "6666666666666666666666666666666666666666",
                           "rebar.config")).

whenchanged_test() ->
    RepDir = code:lib_dir(gitto),
    %% Newest version is last.
    Versions = lists:reverse(rebar_versions(RepDir, ["--first-parent", "-m"])),
    io:format(user, "~nwhenchanged: ~p~n", [dependency_durations(Versions)]).


update_dependencies_test_() ->
    [?_assertEqual(update_dependencies([1,2,3], [2,3,4]),
                   {[1], [2,3], [4]})
    ,?_assertEqual(update_dependencies([1,2,3], [4,5,6]),
                   {[1,2,3], [], [4,5,6]})
    ].

-endif.

