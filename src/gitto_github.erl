-module(gitto_github).
-export([run_import/0]).
-compile([export_all, {parse_transform, lager_transform}]).
-include_lib("gitto/src/gitto.hrl").

-record(detail_rep_info, {
        fullname,
        update_parents_fn,
        source_pair,
        parent_pair,
        rep_info_json,
        is_fork
        }).

-record(import_state, {
        sorted_reps :: [#gh_repository{}],
        parent_dict :: dict(), % FullName => ParentFullName | undefined
        source_dict :: dict(), % FullName => SourceFullName | undefined
        bad_reps_set
}).

import_state() ->
    #import_state{
        sorted_reps = [],
        parent_dict = dict:new(),
        source_dict = dict:new(),
        bad_reps_set = sets:new()
   }.

-type gh_connection() :: term().

run_import() ->
    Con = create_gh_connector(),

    F1 =
         fun(KeyWord) ->
            RepsJSON = gh_lib:extract_all(Con, KeyWord, "erlang", 1),
            lists:ordkeysort(1,
                             [{repository_fullname_json(Rep), Rep} 
                              || Rep <- RepsJSON])
         end,

    %% Search for repos by keywords.
    SortedAddr2RepPairLists = plists:map(F1, keywords(), {processes, 10}),

    %% `X' and `Y' are already sorted.
    F2 = fun(X, Y) -> lists:ukeymerge(1, X, Y) end,

    %% Join them into giant sorted list (12k+ elements).
    SortedAddr2RepPairList = lists:foldl(F2, [], SortedAddr2RepPairLists),

    State1 = analyse_json_reps(SortedAddr2RepPairList, Con, import_state()),
    set_src_dependencies(analyse_deps(Con, State1)).

analyse_deps(Con, State=#import_state{}) ->
    #import_state{sorted_reps = SortedRecs,
                  parent_dict = ParentDict,
                  source_dict = SourceDict,
                  bad_reps_set = BadRepSet} = State,

    IgnoreRepNames = sets:union(BadRepSet,
                                fullname_set(SortedRecs)),
    %% Get a usorted list of dependencies' names.
    case non_downloaded_dependencies(IgnoreRepNames, SortedRecs) of
        [] ->
            State;
        [_|_] = NonDownloadedDeps ->
        lager:info("Deps ~p are not downloaded yet.", [NonDownloadedDeps]),
        %% Get details as JSON for each fullname in `NonDownloadedDeps'.
        DepRepsDetails = plists:map(download_detail_rep_info(Con), 
                                    NonDownloadedDeps, {processes, 10}),

        %% Rep, its source and parent.
        AnotherSortedPairs = select_reps_from_details(DepRepsDetails), 
        BadRepSet2 = sets:union(BadRepSet,
                                filter_bad_reps_set(DepRepsDetails)),
        State2 = #import_state{sorted_reps = SortedRecs,
                               parent_dict = ParentDict,
                               source_dict = SourceDict,
                               bad_reps_set = BadRepSet2},
        State3 = analyse_json_reps(AnotherSortedPairs, Con, State2),
        analyse_deps(Con, State3)
    end.


%% @doc Read info from `{FullName, RepJSON}' pairs.
-spec analyse_json_reps(SortedAddr2RepPairList, Con, State) -> State when
    SortedAddr2RepPairList :: [{FullName, RepJSON}],
    FullName :: gitto_type:gh_repository_fullname(), 
    RepJSON :: term(),
    Con :: gh_connection(),
    State :: #import_state{}.

analyse_json_reps([], _Con, State) ->
    State;
analyse_json_reps(SortedPairs, Con, State) ->
    %% Drop already downloaded pairs.
    NewSortedPairs = lists2:ukeysublist(1, SortedPairs,
                                        #gh_repository.fullname, 
                                        State#import_state.sorted_reps),
    analyse_new_json_reps(NewSortedPairs, Con, State).


analyse_new_json_reps([], _Con, State) ->
    State;
analyse_new_json_reps(NewSortedPairs, Con, State) ->
    F3 =
        fun() ->
        Con2 = create_gh_connector(),
            fun({RepName, X}) ->
                analyse_dependencies(
                    query_and_fill_forks(Con2,
                        query_and_fill_app_structure(Con2, 
                             fill_gh_repository(#gh_repository{fullname = RepName}, X))))
            end
        end,
    SortedRecs0 = plists:init_map(F3, NewSortedPairs, {processes, 25}),

    %% Merge with `State'.
    SortedRecs1 = lists:ukeymerge(#gh_repository.fullname,
                                  SortedRecs0,
                                  State#import_state.sorted_reps),

    ParentDict = dict:merge(fun(_K, X, _Y) -> X end, %% delete duplicates.
                            State#import_state.parent_dict, 
                            parent_repository_dict(SortedRecs1)),

    %% Build a dict to lookup a source (root) repositories.
    %% Use `ParentDict' as a lookup table, while building this dict.
    SourceDict =
    source_repository_dict(ParentDict, SortedRecs1),

    %% There are few repositories, those are not analysed yet.
    SortedRecs2 = lists:map(set_parent_fn(ParentDict), SortedRecs1),
    SortedRecs3 = lists:map(set_source_fn(SourceDict), SortedRecs2),

    %% These repositories have unknown parents.
    LostChildren = 
        [FullName 
         || #gh_repository{fullname = FullName,
                           is_fork = true,
                           parent_fullname = undefined} <- SortedRecs3],
    lager:info("Parents of ~p are not downloaded yet.", [LostChildren]),

    LostChildrenDetails = plists:map(download_detail_rep_info(fun validate_fork/1, Con), 
                                     LostChildren, {processes, 10}),

    %% This dict is used for setting `source_fullname' and 
    %% `parent_fullname' fields.
    Child2ParentSetter = child2parent_setter(LostChildrenDetails),

    %% Update parents for list children.
    SortedRecs4 = lists:map(update_parent_fn(Child2ParentSetter), SortedRecs3),

    %% Few new repositories can be extracted.
    AnotherSortedPairs = select_parents_from_details(LostChildrenDetails), 
    BadOrdRepList = sets:union([State#import_state.bad_reps_set
                               ,filter_bad_reps_set(LostChildrenDetails)]),
    State2 = #import_state{sorted_reps = SortedRecs4,
                           parent_dict = ParentDict,
                           source_dict = SourceDict,
                           bad_reps_set = BadOrdRepList},
    analyse_json_reps(AnotherSortedPairs, Con, State2).



set_parent_fn(ParentDict) ->
    fun(Rep = #gh_repository{is_fork = false}) ->
        Rep;
       (Rep = #gh_repository{fullname = FullName}) ->
        ParentName = parent_fullname(ParentDict, FullName),
        Rep#gh_repository{parent_fullname = ParentName}
    end.


set_source_fn(SourceDict) ->
    fun(Rep = #gh_repository{fullname = FullName,
                             is_fork = true,
                             source_fullname = undefined}) ->
        SourceName = source_fullname(SourceDict, FullName),
        Rep#gh_repository{source_fullname = SourceName};
       (Rep) -> Rep
    end.


keywords() ->
    ["erlang", "library", "application"] ++
    [[X] || X <- lists:seq($a, $b) ++ lists:seq($0, $9)].
%   ["etorrent"].


fill_gh_repository(Rec, [{K, V}|PL]) ->
    Rec2 =
        case K of
            <<"name">> ->
                Rec#gh_repository{name = V};
            <<"description">> ->
                Rec#gh_repository{description = V};
            <<"owner">> ->
                Rec#gh_repository{owner = person_login(V)};
            <<"created">> ->
                Rec#gh_repository{created = datetime_to_timestamp(V)};
            <<"pushed">> ->
                Rec#gh_repository{pushed = datetime_to_timestamp(V)};
            <<"forks">> ->
                Rec#gh_repository{forks_count = V};
            <<"watchers">> ->
                Rec#gh_repository{watchers = V};
            <<"fork">> ->
                Rec#gh_repository{is_fork = V};
            <<"size">> ->
                Rec#gh_repository{size = V};
            _Other ->
                Rec
        end,
    fill_gh_repository(Rec2, PL);
fill_gh_repository(Rec, []) ->
    Rec.


query_and_fill_app_structure(Server,
                             Rec = #gh_repository{name = RepName, 
                                                  owner = RepOwner}) ->
    Props = gh_lib:extract_application_structure(Server, RepOwner, RepName),
    Rec#gh_repository{
        template = detect_template(Props),
        
        has_c_src_dir    = lists:member(has_c_src_dir, Props),
        has_rel_dir      = lists:member(has_rel_dir, Props),
        has_priv_dir     = lists:member(has_priv_dir, Props),
        has_test_dir     = lists:member(has_test_dir, Props),
        rebar_config     = maybe_decode_config(format_error(Rec),
            proplists:get_value(rebar_config, Props))
        }.


query_and_fill_forks(_Server,
                     Rec = #gh_repository{forks_count = 0}) ->
    Rec;
query_and_fill_forks(Server,
                     Rec = #gh_repository{name = RepName, 
                                          owner = RepOwner}) ->
    Rec#gh_repository{forks = gh_lib:fork_addresses(Server, RepOwner, RepName)}.


datetime_to_timestamp(DT) ->
    calendar:datetime_to_gregorian_seconds(iso8601:parse(DT)).


create_gh_connector() ->
    {ok, CacheServer} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, Con} = gh_api:new([{cache_server, CacheServer},
                            {user, "freeakk@gmail.com"},
                            {password, "secret"}]),
    Con.


detect_template(Props) ->
    case lists:member(has_src_dir, Props) of
        true -> src;
        false -> 
            case lists:member(has_apps_dir, Props) of
                true -> apps;
                false -> undefined
            end
    end.

maybe_decode_config(_F, undefined) ->
    undefined;
maybe_decode_config(F, Bin) ->
    try 
        gitto_utils:consult_string(Bin)
    catch Type:Reason ->
        F(Type, Reason),
        undefined
    end.

format_error(#gh_repository{owner = Owner, name = Name}) -> fun(Type, Reason) ->
        lager:error("Error ~p:~p, while consulting rebar.config from ~ts/~ts.",
                    [Type, Reason, Owner, Name])
    end.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

date_convert_test_() ->
    [?_assertEqual(iso8601:parse(<<"2011-11-01T11:35:33-07:00">>),
                   {{2011,11,1},{18,35,33}}),
     ?_assertEqual(calendar:datetime_to_gregorian_seconds({{2011,11,1},{18,35,33}}),
                   63487391733)
    ].

-endif.



%% @doc Return a dict for mapping from a child repository's address to its parent 
%%      repository's address.
parent_repository_dict(Reps) ->
    dict:from_list(parent_repository_proplist(Reps)).


parent_repository_proplist([Rep|Reps]) ->
    ParentAddr = repository_fullname(Rep),
    %% Add a pair.
    [{ForkAddr, ParentAddr} || ForkAddr <- Rep#gh_repository.forks]
    ++ parent_repository_proplist(Reps);
parent_repository_proplist([]) ->
    [].


%repository_fullname(#gh_repository{owner = Owner, name = Name}) ->
%   {Owner, Name}.
repository_fullname(#gh_repository{fullname = FullName}) ->
    FullName.


repository_fullname_json(Rep) ->
    Owner = person_login(proplists:get_value(<<"owner">>, Rep)),
    Name  = proplists:get_value(<<"name">>, Rep),
    {Owner, Name}.


maybe_repository_fullname_json(undefined) ->
    undefined;
maybe_repository_fullname_json(Rep) ->
    repository_fullname_json(Rep).
    

person_login(OwnerLogin) when is_binary(OwnerLogin) ->
    OwnerLogin;
person_login(OwnerJSON) ->
    proplists:get_value(<<"login">>, OwnerJSON).


parent_fullname(ParentDict, RepName) ->
    case dict:find(RepName, ParentDict) of
        {ok, Parent} -> Parent;
        error -> undefined
    end.


%% @doc If this repository is a source, than this function just returns the
%%      `RepName' argument.
source_fullname_using_parent_dict(ParentDict, RepName) ->
    case parent_fullname(ParentDict, RepName) of
        undefined -> RepName;
        ParentName -> source_fullname_using_parent_dict(ParentDict, ParentName)
    end.


source_repository_dict(ParentDict, Recs) ->
    Pairs =
    [{FullName, source_fullname_using_parent_dict(ParentDict, FullName)}
     || #gh_repository{is_fork = true, fullname = FullName} <- Recs],

    %% Delete all `{X, X}' pairs.
    FilteredPairs =
    [Addr
     || Addr = {X, Y} <- Pairs,    X =/= Y],

    dict:from_list(FilteredPairs).


source_fullname(SourceDict, RepName) ->
    parent_fullname(SourceDict, RepName).


root_fullname(SourceDict, RepName) ->
    case source_fullname(SourceDict, RepName) of
        %% The current repository is a source.
        undefined -> RepName;
        SourceName -> SourceName
    end.

%% ------------------------------------------------------------------
%% Deps
%% ------------------------------------------------------------------

analyse_dependencies(Rec = #gh_repository{rebar_config = undefined}) ->
    Rec;
analyse_dependencies(Rec = #gh_repository{rebar_config = RebarConfig}) ->
    case proplists:get_value(deps, RebarConfig) of
        undefined -> Rec;
        Deps -> 
            try
                Rec#gh_repository{dependencies = decode_rebar_deps(Deps)}
            catch error:Reason ->
                lager:error("Bad formed deps section of 'rebar.config' " % ++
                            "of the '~p' repository.~nReason: ~p",
                            [Rec#gh_repository.fullname, Reason]),
                Rec
            end
    end.


decode_rebar_deps(Deps) ->
    DepsOnGH =
    [url_to_repository_fullname(RepURL)
     || {_AppName, _VersionPattern, {git, RepURL, _Rev}} <- Deps],
    [Dep
     || Dep <- DepsOnGH, Dep =/= undefined].


url_to_repository_fullname(URL) ->
    %% This is a second problem.
    case re:run(URL, <<"^(git|https|http)://github.com/([^/]+)/(.+?)(\\.git|)$">>, 
                [{capture, [2,3], binary}]) of
        {match, [OwnerLogin, RepName]} ->
            {OwnerLogin, RepName};
        nomatch ->
            undefined
    end.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

url_to_repository_fullname_test_() ->
    [?_assertEqual(url_to_repository_fullname("git://github.com/arcusfelis/plists.git"),
                   {<<"arcusfelis">>, <<"plists">>})
    ,?_assertEqual(url_to_repository_fullname("git://github.com/arcusfelis/plists"),
                   {<<"arcusfelis">>, <<"plists">>})
    ].


decode_rebar_deps_test_() ->
    Deps = [{plists, ".*", {git, "git://github.com/arcusfelis/plists.git", "HEAD"}}
           ,{octoerl, ".*", {git, "git://github.com/arcusfelis/octoerl.git", "HEAD"}}

           %% HTTP sucks.
           ,{gen_server2, ".*", {git, "http://github.com/hyperthunk/gen_server2.git", "master"}}],
    [?_assertEqual(decode_rebar_deps(Deps),
                                     [{<<"arcusfelis">>, <<"plists">>}
                                     ,{<<"arcusfelis">>, <<"octoerl">>}
                                     ,{<<"hyperthunk">>, <<"gen_server2">>}])].
                                     
       

-endif.

fullname_set(Recs) ->
    sets:from_list([FullName || #gh_repository{fullname = FullName} <- Recs]).


is_repository_downloaded(FullName, FullNames) ->
    sets:is_element(FullName, FullNames).


non_downloaded_dependencies(FullNames, Recs) ->
    [DepFullName
     || #gh_repository{dependencies = Deps} <- Recs,
        DepFullName <- Deps,
        not is_repository_downloaded(DepFullName, FullNames)].


update_parent_fn(Child2ParentSetter) ->
    %% Is this rep is lost child?
    fun(Rec=#gh_repository{fullname = FullName,
                           is_fork = true,
                           parent_fullname = undefined}) ->
        case dict:find(FullName, Child2ParentSetter) of
            %% Update `Rec'.
            {ok, F} -> F(Rec);
            %% Something is wrong!
            error -> 
                lager:error("FIXME: Cannot select an updater for ~p.",
                            [FullName]),
                Rec
        end;
       (Rec) -> Rec
    end.


validate_fork(#detail_rep_info{is_fork = true}) -> ok;
validate_fork(#detail_rep_info{is_fork = false}) -> {error, not_fork}.


download_detail_rep_info(Con) ->
     download_detail_rep_info(fun(_) -> ok end, Con).

download_detail_rep_info(ValidatorF, Con) ->
    fun({OwnerLogin, RepName} = FullName) ->
        case gh_api:get_repository(Con, OwnerLogin, RepName) of
        {ok, undefined} ->
            {error, {gh_error, FullName, 404}};
        {ok, DetailRepInfo} ->
            ParentRepJSON = proplists:get_value(<<"parent">>, DetailRepInfo),
            SourceRepJSON = proplists:get_value(<<"source">>, DetailRepInfo),
            IsFork        = proplists:get_value(<<"fork">>, DetailRepInfo),
            SourceFullName = maybe_repository_fullname_json(SourceRepJSON),
            ParentFullName = maybe_repository_fullname_json(ParentRepJSON),
            %% Function for updating.
            F = fun(Rec=#gh_repository{}) ->
                    Rec#gh_repository{
                        source_fullname = SourceFullName,
                        parent_fullname = ParentFullName}
                end,

            Info = #detail_rep_info{
                fullname = FullName,
                update_parents_fn = F,
                source_pair = maybe_pair({SourceFullName, SourceRepJSON}),
                parent_pair = maybe_pair({ParentFullName, ParentRepJSON}),
                rep_info_json = DetailRepInfo,
                is_fork = IsFork},
            case ValidatorF(Info) of
                ok -> {ok, Info};
                {error, Reason} -> 
                    lager:error("~p is invalid.~nReason: ~p~n", 
                                [FullName, Reason]),
                    {error, {gh_error, FullName, Reason}}
            end;

        {error, Reason} ->
            lager:error("~p looks like a dead repository.~nReason: ~p~n", 
                        [FullName, Reason]),
            {error, {gh_error, FullName, Reason}}
        end
    end.


%% Returns ordeset.
filter_bad_reps_set(List) ->
    sets:from_list(filter_bad_reps(List)).


filter_bad_reps(List) ->
    [FullName
     || {error, {gh_error, FullName, _Reason}} <- List].


%% Fails, if not a fork.
select_parents_from_details(MoreInfoForLostChildren) ->
    F = fun({ok, #detail_rep_info{source_pair = Source, 
                                  parent_pair = Parent,
                                  is_fork = true}})
                when Source =/= undefined, Parent =/= undefined ->
            [Source, Parent];
           ({error, _Reason}) ->
            []
        end,
    Pairs = lists:flatmap(F, MoreInfoForLostChildren),

    %% Sorted, unique, but can be already downloaded into `SortedRecs@'.
    lists:ukeysort(1, Pairs).


select_reps_from_details(MoreInfoForLostChildren) ->
    F = fun({ok, #detail_rep_info{source_pair = Source, 
                                  parent_pair = Parent,
                                  fullname = FullName,
                                  rep_info_json = DetailRepInfo}}) 
                when FullName =/= undefined ->
            MaybeReps = [{FullName, DetailRepInfo}, Source, Parent],
            [Rep || Rep <- MaybeReps, Rep =/= undefined];
           ({error, _Reason}) ->
            []
        end,
    Pairs = lists:flatmap(F, MoreInfoForLostChildren),

    %% Sorted, unique, but can be already downloaded into `SortedRecs@'.
    lists:ukeysort(1, Pairs).


child2parent_setter(MoreInfoForLostChildren) ->
    dict:from_list([{FullName, F}
                    || {ok, #detail_rep_info{fullname = FullName,
                                             update_parents_fn = F}}
                       <- MoreInfoForLostChildren]).


maybe_pair({undefined, undefined}) ->
    undefined;
maybe_pair({_X, _Y} = XY) ->
    XY.


set_src_dependencies(State=#import_state{}) ->
    #import_state{sorted_reps = SortedRecs,
                  source_dict = SourceDict} = State,
    SortedRecs2 =
      [Rep#gh_repository{src_dependencies = reps_sources(SourceDict, Deps)}
         || Rep=#gh_repository{dependencies = Deps} <- SortedRecs],

    State#import_state{sorted_reps = SortedRecs2}.


reps_sources(SourceDict, Deps) ->
    [root_fullname(SourceDict, Dep) || Dep <- Deps].
