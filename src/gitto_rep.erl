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
    mycmd:cmd("git", ["tag", "--", Comment],  [{cd, RepDir}]).

merge(RepDir, Version) ->
    mycmd:cmd("git", ["merge", "--", Version],  [{cd, RepDir}]).

%% Clone from `RepURL' to `RepURL'.
%% git clone --bare
bare_clone(RepURL, RepDir) ->
    mycmd:cmd("git", ["clone", "--bare", "--", RepURL, RepDir]).

clone(RepURL, RepDir) ->
    mycmd:cmd("git", ["clone", "--", RepURL, RepDir]).

%% @doc Pull from `RepURL' to `RepDir'.
pull(RepURL, RepDir) ->
    mycmd:cmd("git", ["pull", "--", RepURL],  [{cd, RepDir}]).

%% git clone --shared
checkout(BareRepDir, TargetRepDir, Revision) -> 
    mycmd:cmd("git", ["clone", "--shared", "--", BareRepDir, TargetRepDir]),
    mycmd:cmd("git", ["checkout", Revision],
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
               ["whatchanged", "--format=%H"] ++ Flags ++ ["--", FileName], 
               [{cd, RepDir}]),
    iolist_to_binary(Data).

%% gitto_whenchanged(Str)


first_parent(RepDir) ->
    first_parents(RepDir, []).

first_parents(RepDir, Flags) ->
    Data = cmd("git", 
               ["log", "--format=%H", "--first-parent"] ++ Flags, 
               [{cd, RepDir}]),
    gitto_utils:parse_commit_hashes(iolist_to_binary(Data)).


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
    RevList = gitto_utils:parse_commit_hashes(whenchanged(RepDir, FN, Flags)),
    [{RevNum, 
      try {ok, read_file(RepDir, binary_to_list(RevNum), FN)}
      catch error:{bad_exit_status, _} = Reason -> {error, Reason}
      end} || RevNum <- RevList].


versions_and_files(RepDir, FNPattern, Flags) ->
    RevList = gitto_utils:parse_commit_hashes_and_filenames(
            whenchanged(RepDir, FNPattern, Flags)),
    [{RevNum, 
      FN,
      %% TODO: binary_to_iolist or unicode:binary_to_characters?
      try {ok, read_file(RepDir, binary_to_list(RevNum), binary_to_list(FN))}
      catch error:{bad_exit_status, _} = Reason -> {error, Reason}
      end} || {RevNum, FileNames} <- RevList,
              FN <- FileNames].

rebar_config_versions(RepDir, Flags) ->
    Configs = versions(RepDir, "rebar.config", Flags),
    F = fun(Data) ->
            proplists:get_value(deps, gitto_utils:consult_string(Data))
            end,
    [{RevNum, maybe_with(F, Result)}
        || {RevNum, Result} <- Configs].


app_config_versions(RepDir, Flags) ->
    Configs = versions_and_files(RepDir, "src/*.app.src", Flags),
    [{RevNum, FN, maybe_with(fun gitto_utils:consult_string/1, Data)}
        || {RevNum, FN, Data} <- Configs].


maybe_with(F, {ok, X}) -> {ok, F(X)};
maybe_with(_F, {error, _} = E) -> E.


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
    %% Merges are included.
    Versions = lists:reverse(rebar_config_versions(RepDir, ["--first-parent", "-m"])),
    io:format(user, "~nwhenchanged: ~p~n", [Versions]).


-endif.

