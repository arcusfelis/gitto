-module(gitto_deps_gexf).
-export([build_xml/1, enumerate/1]).
-include_lib("gitto/src/gitto.hrl").
-compile({parse_transform, lager_transform}).


build_xml(Reps) ->
%   lager:info("Reps: ~p", [Reps]),
    lager:info("Number of all repositories: ~p", [length(Reps)]),

    SrcReps = filter_popular(drop_forks(Reps)),
    lager:info("Number of source repositories: ~p", [length(SrcReps)]),

    LinkedSet = linked_set(SrcReps),
    SrcReps2 = drop_unlinked(LinkedSet, SrcReps),


    SrcNameSet = sets:from_list(lists2:keys(#gh_repository.fullname, SrcReps2)),

    %% `SrcReps3' has the same length of the list, but this step decreases the 
    %% count dependencies for each rep.
    SrcReps3 = enumerate(validate_dependencies(SrcReps2, SrcNameSet)),

    FullName2Id = dict:from_list([{FullName, Id}
                                  || #gh_repository{id = Id, fullname = FullName}
                                     <- SrcReps3]),

    %% Fullname to dep pairs.
    ParentChildPairs = [{FullName, Dep}
                        || #gh_repository{fullname = FullName,
                                          src_dependencies = Deps} <- SrcReps3,
                           Dep <- Deps],

    Nodes = [rep_node(Rep) || Rep <- SrcReps3],
    Edges = [dep_edge(FullName2Id, EdgeId, FullName, Dep)
             || {EdgeId, {FullName, Dep}} <- lists2:enumerate(ParentChildPairs)],
    Graph = gexf:graph(Nodes, Edges),
    gexf:document_viz(Graph).


%% @doc Save only known dependencies.
validate_dependencies(SrcReps, SrcNameSet) ->
    IsKnownRep = fun(RepName) -> sets:is_element(RepName, SrcNameSet) end,
    lists:keymap(fun(Deps) -> lists:filter(IsKnownRep, Deps) end,
                 #gh_repository.src_dependencies,
                 SrcReps).


drop_forks(Reps) ->
    [Rep
     || Rep=#gh_repository{is_fork = false} <- Reps].


%% @doc Return a set of names of repositories that are interconnected.
linked_set(Reps) ->
    DepsList = lists2:keys(#gh_repository.src_dependencies, Reps),
    %% Reps, those have dependencies.
    Names = [FN
             || #gh_repository{fullname = FN, 
                               src_dependencies = Deps} <- Reps,
                Deps =/= []],
    sets:from_list(lists:merge([Names|DepsList])).


drop_unlinked(LinkedSet, Reps) ->
    [Rep
     || Rep=#gh_repository{fullname = FN} <- Reps, sets:is_element(FN, LinkedSet)].


filter_popular(Reps) ->
    [Rep
     || Rep=#gh_repository{watchers = W} <- Reps, W > 3].



rep_node(Rep=#gh_repository{id = Id, fullname = FullName}) ->
    gexf:add_size(rep_node_size(Rep),
                  gexf:add_color(get_color(FullName),
                                 gexf:set_label(print_fullname(FullName), 
                                                gexf:node(Id)))).


rep_node_size(#gh_repository{watchers = Watchers}) ->
    gexf:size(watchers_count_to_node_size(Watchers)).


watchers_count_to_node_size(Watchers) when Watchers > 150 -> <<"4.5">>;
watchers_count_to_node_size(Watchers) when Watchers > 80  -> <<"4">>;
watchers_count_to_node_size(Watchers) when Watchers > 50  -> <<"3.5">>;
watchers_count_to_node_size(Watchers) when Watchers > 25  -> <<"3">>;
watchers_count_to_node_size(Watchers) when Watchers > 10  -> <<"2.5">>;
watchers_count_to_node_size(_Watchers)                    -> <<"2">>.


dep_edge(FullName2Id, EdgeId, FullName, DepFullName) ->
    RepId = fun(RepName) -> dict:fetch(RepName, FullName2Id) end,
    FromNode = RepId(FullName),
    ToNode   = RepId(DepFullName),
    gexf:edge(EdgeId, FromNode, ToNode).


print_fullname({Owner, RepName}) ->
    io_lib:format("~ts ~ts", [Owner, RepName]).


get_color({Owner, _RepName}) ->
    <<R, G, B, _/binary>> = brighter_colors(128, crypto:md5(Owner)),
    gexf:color(R, G, B).


brighter_colors(Mask, Colors) ->
    << <<(X bor Mask)>> || <<X>> <= Colors>>.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_xml_test() ->
    lager:start(),
    FileName = filename:join(code:priv_dir(gitto), "reps.bin"),
    {ok, RepsBin} = file:read_file(FileName),
    Xml = build_xml(binary_to_term(RepsBin)),
    io:format(user, "Xml: ~p", [Xml]),
    gexf:to_string(Xml),
    ok.

-endif.

enumerate(L) ->
    enumerate(L, 1).

enumerate([H|T], X) ->
    [H#gh_repository{id = X}|enumerate(T, X+1)];
enumerate([], _X) ->
    [].



