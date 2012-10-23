%% @doc This module provides non-pure operations.
%% It knows about DB through `gitto_db' and `gitto_store'.
%% It calls commands from `gitto_rep'.
%% This module encapsulates the structure of directories.
-module(gitto_exec).

%% Download
-export([download/2]).

%% Log
-export([log/2]).

%% First parent
-export([first_parent/2]).

%% Configs
-export([rebar_config_versions/2,
         app_config_versions/2]).

%% Checkout
-export([checkout_revision/3]).

%% Rebar
-export([compile_revision/2,
         link_dependency/3]).


%% ------------------------------------------------------------------
%% Download
%% ------------------------------------------------------------------

download(Cfg, Rep) ->
    Addrs = gitto_db:select(gitto_store:repository_addresses(Rep)),
    LocalPath = local_repository_path(Cfg, Rep),
    download_one_of(Addrs, LocalPath).


local_repository_path(Cfg, Rep) ->
    filename:join(gitto_config:lookup_value(bare_reps_dir, Cfg),
                  gitto_store:repository_literal_id(Rep)).


local_revision_path(Cfg, Rev) ->
    filename:join(gitto_config:lookup_value(rev_reps_dir, Cfg),
                  gitto_store:revision_literal_id(Rev)).

%% @doc Same, but gets a path of a subdirectory, i.e. `["deps", "cowboy"]'.
local_revision_path(Cfg, Rev, Tail) ->
    filename:join([local_revision_path(Cfg, Rev) | Tail]).


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
        gitto_store:update_downloading_date(Addr),
        Addr
        catch Type:Error ->
        gitto_store:update_trying_date(Addr),
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


%% ------------------------------------------------------------------
%% First Parent
%% ------------------------------------------------------------------

first_parent(Cfg, Rep) ->
    LocalPath = local_repository_path(Cfg, Rep),
    gitto_rep:first_parent(LocalPath).


%% ------------------------------------------------------------------
%% Rebar config
%% ------------------------------------------------------------------

rebar_config_versions(Cfg, Rep) ->
    LocalPath = local_repository_path(Cfg, Rep),
    gitto_rep:rebar_config_versions(LocalPath, ["--first-parent", "-m"]).

app_config_versions(Cfg, Rep) ->
    LocalPath = local_repository_path(Cfg, Rep),
    gitto_rep:app_config_versions(LocalPath, ["-m"]).


%% ------------------------------------------------------------------
%% Checkout
%% ------------------------------------------------------------------

checkout_revision(Cfg, Rep, Rev) ->
    BareRepDir   = local_repository_path(Cfg, Rep),
    TargetRepDir = local_revision_path(Cfg, Rev),
    Revision     = gitto_store:revision_hash(Rev),
    gitto_rep:checkout(BareRepDir, TargetRepDir, Revision),
    gitto_store:downloading_completed(Rev),
    ok.


%% ------------------------------------------------------------------
%% Rebar
%% ------------------------------------------------------------------

compile_revision(Cfg, Rev) ->
    RevDir = local_revision_path(Cfg, Rev),
    assert_directory(RevDir),
    gitto_rebar:compile(RevDir),
    ok.

link_dependency(Cfg, Dep, LinkTargetRevId) ->
    S = local_revision_path(Cfg, gitto_store:dependency_donor_repository_id(Dep)),
    T = local_revision_path(Cfg, LinkTargetRevId, 
                            ["deps", gitto_store:dependency_name(Dep)]),
    %% Is the `deps' directory exist?
    assert_directory(S),
    ok = filelib:ensure_dir(T),
    Result = file:make_symlink(S, T),
    error_logger:info_msg("Link ~p with ~p returns **~p**.~n", [S, T, Result]),
    Result.


assert_directory(Dir) ->
    [erlang:error({assert_directory, Dir}) || not filelib:is_dir(Dir)].
    
