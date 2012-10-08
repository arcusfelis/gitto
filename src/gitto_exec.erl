%% @doc This module provides non-pure operations.
%% It knows about DB through `gitto_db' and `gitto_store'.
%% It calls commands from `gitto_rep'.
%% This module encapsulates the structure of directories.
-module(gitto_exec).

%% Download
-export([download/2]).

%% Log
-export([log/2]).


%% ------------------------------------------------------------------
%% Download
%% ------------------------------------------------------------------

download(Cfg, Rep) ->
    Addrs = gitto_db:select(gitto_store:repository_addresses(Rep)),
    LocalPath = local_repository_path(Cfg, Rep),
    download_one_of(Addrs, LocalPath).


local_repository_path(Cfg, Rep) ->
    filename:join(gitto_config:get_value(bare_reps_dir, Cfg),
                  gitto_store:repository_literal_id(Rep)).



-spec download_one_of([Addr], LocalPath) -> Addr | undefined
    when
    LocalPath :: filename:path(),
    Addr :: gitto_type:address().


download_one_of([], _LocalPath) ->
    error_logger:error_msg("Downloading error: no addresses.~n", []),
    undefined;

download_one_of([Addr|Addrs], LocalPath) ->
    try
        gitto_rep:bare_clone(gitto_store:address_to_url(Addr), LocalPath),
        Addr
        catch Type:Error ->
        error_logger:error_msg("Downloading error ~p:~p from ~p.~n",
                               [Type, Error, Addr]),
        download_one_of(Addrs, LocalPath)
    end.


%% ------------------------------------------------------------------
%% Log
%% ------------------------------------------------------------------

log(Cfg, Rep) ->
    LocalPath = local_repository_path(Cfg, Rep),
    gitto_rep:log(LocalPath).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

download_test() ->
    gitto_db:up(),
    Cfg = gitto_config:example(),
    gitto_config:get_value(bare_reps_dir, Cfg),
    
    Project = gitto_db:write(gitto_store:project([{name, download_test}])),
    ProjectId = gitto_store:to_id(Project),
    io:format(user, "Project: ~p~n", [Project]),

    RepCon = [{project, ProjectId}],
    Rep = gitto_db:write(gitto_store:repository(RepCon)),
    RepId = gitto_store:to_id(Rep),

    %% Create an empty repository
    %% An example of the Url is `data/test_reps/download_test4'.
    Url = filename:join(gitto_config:get_value(test_reps_dir, Cfg),
                        [download_test, integer_to_list(ProjectId)]),
    create_example_repository(Url),
    AddrCon = [{repository, gitto_store:to_id(Rep)}, {url, Url}],
    Addr = gitto_db:write(gitto_store:address(AddrCon)),

    %% Test one: try to clone.
    ?MODULE:download(Cfg, Rep),

    %% Test two: try to extract metainformation.
    Commits = lists:reverse(gitto_log:parse_commits(?MODULE:log(Cfg, Rep))),
    io:format(user, "Reversed commits: ~p~n", [Commits]),

    %% Author and committer will be added in the DB automatically with the
    %% `improper_revision' call.
    %%
    %% Each Commit is a proplist.
    %% Returns persisted revisions.
    Revisions = 
        [gitto_db:write(
            gitto_store:revision(
                gitto_store:improper_revision(Commit)))
            || Commit <- Commits],
    RRs = 
        [gitto_db:write(gitto_store:repository_x_revision(Rep, Rev, true))
            || Rev <- Revisions],
    RDIs = 
        [gitto_db:write(gitto_store:revision_date_index(Rep, Rev))
            || Rev <- Revisions],
    io:format(user, "Revisions: ~p~n", [Revisions]),

    ok.


create_example_repository(Url) ->
    error_logger:info_msg("Create an example repository: ~ts", [Url]),
    ok = file:make_dir(Url),
    ok = file:write_file(filename:join(Url, "README"), "This is am example repository."),

    gitto_rep:init(Url),
    gitto_rep:add_all(Url),
    gitto_rep:commit(Url, "First commit."),
    ok.


create_example_branching_repository_test() ->
    gitto_db:up(),
    Cfg = gitto_config:example(),
    gitto_config:get_value(bare_reps_dir, Cfg),
    
    Project = gitto_db:write(gitto_store:project([{name, branching_test}])),
    ProjectId = gitto_store:to_id(Project),
    io:format(user, "Project: ~p~n", [Project]),

    RepCon = [{project, ProjectId}],
    Rep = gitto_db:write(gitto_store:repository(RepCon)),

    %% Create an empty repository
    %% An example of the Url is `data/test_reps/branching_test4'.
    Url = filename:join(gitto_config:get_value(test_reps_dir, Cfg),
                        [branching_test, integer_to_list(ProjectId)]),
    RepDir = create_example_branching_repository(Url),

%%  lists:reverse(gitto_rep:rebar_versions(RepDir, ["--first-parent", "-m"])),
    ok.


create_example_branching_repository(Url) ->
    %% Create a directory structure:
    %% + Url/
    %% |-Url/fork1
    %% |-Url/fork2
    Url1 = filename:join(Url, "fork1"),
    Url2 = filename:join(Url, "fork2"),

    error_logger:info_msg("Create an example repository: ~ts~n", [Url1]),

    ok = file:make_dir(Url),
    ok = file:make_dir(Url1),

    ok = file:write_file(filename:join(Url1, "README"), "Version 1."),

    gitto_rep:init(Url1),
    gitto_rep:add_all(Url1),
    gitto_rep:commit(Url1, "Commit 1."),
    gitto_rep:tag(Url1, "C1"),

    error_logger:info_msg("Fork an example repository: ~ts => ~ts", [Url1, Url2]),
    gitto_rep:clone(Url1, Url2),

    ok = file:write_file(filename:join(Url1, "README"), "Version 2."),
    ok = file:write_file(filename:join(Url2, "README"), "Version 3."),

    gitto_rep:add_all(Url1),
    gitto_rep:add_all(Url2),

    gitto_rep:commit(Url1, "Commit 2."),
    gitto_rep:commit(Url2, "Commit 3."),

    gitto_rep:tag(Url1, "C2"),
    gitto_rep:tag(Url2, "C3"), 

    %% Merge fork1 with fork2.
    gitto_rep:pull(Url2, Url1), 

    ok = file:write_file(filename:join(Url1, "README"), "Version 3."),
    gitto_rep:add_all(Url1),
    gitto_rep:commit(Url1, "Commit 4."),
    gitto_rep:tag(Url1, "C4"), 

    Url1.



create_example_with_rebar_dependiencies_test() ->
    gitto_db:up(),
    Cfg = gitto_config:example(),
    gitto_config:get_value(bare_reps_dir, Cfg),
    
    Project = gitto_db:write(gitto_store:project([{name, deps_test}])),
    ProjectId = gitto_store:to_id(Project),
    io:format(user, "Project: ~p~n", [Project]),

    RepCon = [{project, ProjectId}],
    Rep = gitto_db:write(gitto_store:repository(RepCon)),

    %% Create an empty repository
    %% An example of the Url is `data/test_reps/deps_test4'.
    Url = filename:join(gitto_config:get_value(test_reps_dir, Cfg),
                        [deps_test, integer_to_list(ProjectId)]),
    RepDir = create_example_with_rebar_dependiencies(Url),

    Versions = 
    lists:reverse(gitto_rep:rebar_config_versions(RepDir, ["--first-parent", "-m"])),
    io:format(user, "~nVersions: ~p~n", [Versions]),

    ok.


create_example_with_rebar_dependiencies(Url) ->
    %% Create a directory structure:
    %% + Url/
    %% |-Url/app1
    %% |-Url/app2
    Url1 = filename:join(Url, "app1"),
    Url2 = filename:join(Url, "app2"),
    Url3 = filename:join(Url, "app3"),

    error_logger:info_msg("Create an example repository: ~ts~n", [Url1]),
    error_logger:info_msg("Create an example repository: ~ts~n", [Url2]),
    error_logger:info_msg("Create an example repository: ~ts~n", [Url3]),

    ok = file:make_dir(Url),
    ok = file:make_dir(Url1),
    ok = file:make_dir(Url2),
    ok = file:make_dir(Url3),

    Deps1_1 = [{app2, ".*", {git, "../app2", "HEAD"}}],
    Deps1_2 = [{app2, ".*", {git, "../app2", "HEAD"}},
               {app3, ".*", {git, "../app3", "HEAD"}}],

    Cfg1_1 = io_lib:format("{deps, ~p}.", [Deps1_1]),
    Cfg1_2 = io_lib:format("{deps, ~p}.", [Deps1_2]),
    Cfg2_1 = Cfg3_1 = "{deps, []}.",
    ok = file:write_file(filename:join(Url1, "rebar.config"), Cfg1_1),
    ok = file:write_file(filename:join(Url2, "rebar.config"), Cfg2_1),
    ok = file:write_file(filename:join(Url3, "rebar.config"), Cfg3_1),

    gitto_rep:init(Url1),
    gitto_rep:add_all(Url1),
    gitto_rep:commit(Url1, "Application 1. Commit 1."),
    gitto_rep:tag(Url1, "A1C1"),

    gitto_rep:init(Url2),
    gitto_rep:add_all(Url2),
    gitto_rep:commit(Url2, "Application 2. Commit 1."),
    gitto_rep:tag(Url2, "A2C1"),

    gitto_rep:init(Url3),
    gitto_rep:add_all(Url3),
    gitto_rep:commit(Url3, "Application 3. Commit 1."),
    gitto_rep:tag(Url3, "A3C1"),

    ok = file:write_file(filename:join(Url1, "rebar.config"), Cfg1_2),
    gitto_rep:add_all(Url1),
    gitto_rep:commit(Url1, "Application 1. Commit 2."),
    gitto_rep:tag(Url1, "A1C2"), 

    Url1.

-endif.

