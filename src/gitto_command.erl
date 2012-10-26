-module(gitto_command).
-compile([export_all, {parse_transform, lager_transform}]).


parse_and_save(Cfg, Rep) ->
    lager:info("Start parsing and saving: ~p~n", [Rep]),

    %% Test two: try to extract metainformation.
    Commits = lists:reverse(gitto_log:parse_commits(gitto_exec:log(Cfg, Rep))),
    lager:info("Reversed commits: ~p~n", [Commits]),
    
    FirstParentHashes = gitto_exec:first_parent(Cfg, Rep),
    lager:info("First parents: ~p~n", [FirstParentHashes]),
    
    IsFirstParent = fun(Rev) -> 
            Hash = gitto_store:revision_hash(Rev),
            lists:member(Hash, FirstParentHashes)
        end,

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
    %% Create connectings beetween the repository and revisions.
    RRs = 
        [gitto_db:write(
            gitto_store:repository_x_revision(Rep, Rev, IsFirstParent(Rev)))
            || Rev <- Revisions],
    %% Create date indexes.
    RDIs = 
        [gitto_db:write(gitto_store:revision_date_index(Rep, Rev))
            || Rev <- Revisions],
    lager:info("Revisions: ~p~n", [Revisions]),
    lager:info("Stop parsing and saving: ~p~n", [Rep]),

    ok.

analyse_dependencies(Cfg, Rep) ->
    Versions = lists:reverse(gitto_exec:rebar_config_versions(Cfg, Rep)),
    lager:info("Versions: ~p~n", [Versions]),
    
    %% 2 nested generators, filter `{RevHash, undefined}'.
    Deps =
    [gitto_db:write(
        gitto_store:dependency(Dep, gitto_store:lookup_revision(RevHash)))
        || {RevHash, {ok, Deps}} <- Versions, Deps =/= undefined, Dep <- Deps],
    lager:info("Deps: ~p~n", [Deps]),
    ok.


%% @doc Load `app.src' for all revisions of the repository.
analyse_app_config(Cfg, Rep) ->
    Versions = lists:reverse(gitto_exec:app_config_versions(Cfg, Rep)),
    lager:info("app.src versions: ~p.~n", [Versions]),
    RevWrittenDecodedApps = 
    [{gitto_store:lookup_revision(RevHash),
      gitto_store:write_encoded_application(App)}
     || {RevHash, _FileName, {ok, [App]}} <- Versions],

    UpdatedRevisions =
    [gitto_store:update_revision_application(gitto_store:to_id(RevId), 
                                             gitto_store:to_id(App))
     || {RevId, App} <- RevWrittenDecodedApps],


    lager:info("app.src written versions: ~p.~n", 
              [RevWrittenDecodedApps]),

    lager:info("~nSet application for next revisions: ~p.~n", 
              [UpdatedRevisions]),
    ok.


download_dependencies(Cfg, Rep) ->
    %% Get first-level dependencies (deps of the current app, not deps of deps).
    MissingDeps = gitto_store:missing_dependencies(Rep),
    DonorRepsAndUdefs =
        lists:usort([gitto_store:dependency_to_donor_repository(Dep) 
                     || Dep <- MissingDeps]),
    DonorReps =
        [DonorRep || DonorRep <- DonorRepsAndUdefs, DonorRep =/= undefined],
    lager:info("Missing deps: ~p~n from ~p.~n", 
              [MissingDeps, DonorReps]),
    %% TODO: Should be done parallelly.
    [begin
        gitto_exec:download(Cfg, DonorRep),
        parse_and_save(Cfg, DonorRep)
     end
     || DonorRep <- DonorReps],
    FixedDeps = [update(Dep, gitto_store:fix_dependency_donor(Dep))
                 || Dep <- MissingDeps],
    lager:info("Fixed deps: ~p.~nOld deps: ~p.~n", 
              [FixedDeps, MissingDeps]),
    ok.

update(Old, Old) ->
    Old;
update(_Old, New) ->
    gitto_db:write(New).



handle_project(Cfg, ProjectName, RepDir) ->
    lager:info("Download project ~p from ~p.~n", [ProjectName, RepDir]),

    Project = gitto_db:write(gitto_store:project([{name, ProjectName}])),
    ProjectId = gitto_store:to_id(Project),
    lager:info("Project: ~p~n", [Project]),

    RepCon = [{project, ProjectId}],
    Rep = gitto_db:write(gitto_store:repository(RepCon)),

    AddrCon = [{repository, gitto_store:to_id(Rep)}, {url, RepDir}],
    Addr = gitto_db:write(gitto_store:address(AddrCon)),

    gitto_exec:download(Cfg, Rep),
    parse_and_save(Cfg, Rep),
    analyse_app_config(Cfg, Rep),
    analyse_dependencies(Cfg, Rep),
    download_dependencies(Cfg, Rep),
    Rev = gitto_store:latest_revision_number(gitto_store:to_id(Rep)),
    checkout_revision(Cfg, Rev),
    get_revision_dependencies(Cfg, Rev),
    compose_revision(Cfg, Rev),
    compile_revision(Cfg, Rev),
    ok.




compose_revision(Cfg, RevId) ->
    lager:info("Compose revision #~p.~n", [RevId]),
    Deps = gitto_store:recursively_lookup_dependencies(RevId),
    [gitto_exec:link_dependency(Cfg, Dep, RevId) || Dep <- Deps],
    lager:info("Wanted deps ~p.~n", [Deps]),
    ok.


%% @doc Copy and compile revision dependencies.
get_revision_dependencies(Cfg, RevId) ->
    Deps = gitto_store:recursively_lookup_dependencies(RevId),
    [begin
         DonorRepId = gitto_store:dependency_donor_repository_id(Dep),
         checkout_revision(Cfg, DonorRepId),
         compose_revision(Cfg, DonorRepId) 
     end
     || Dep <- Deps],
    ok.


%% @doc Copy revision specified files from the bare repository.
checkout_revision(Cfg, RevId) ->
    lager:info("Checkout revision #~p.~n", [RevId]),
    RepId = gitto_store:revision_to_repository(RevId),
    [erlang:error({cannot_get_revision_repository, RevId}) 
     || RepId =:= undefined],
    Rep = gitto_store:get_repository(RepId),
    Rev = gitto_store:get_revision(RevId),
    gitto_exec:checkout_revision(Cfg, Rep, Rev),
    ok.

%% @doc Run `rebar compile' for the checked out application.
compile_revision(Cfg, RevId) ->
    lager:info("Checkout revision #~p.~n", [RevId]),
    RepId = gitto_store:revision_to_repository(RevId),
    [erlang:error({cannot_get_revision_repository, RevId}) 
     || RepId =:= undefined],
    Rev = gitto_store:get_revision(RevId),
    gitto_exec:compile_revision(Cfg, Rev),
    ok.


