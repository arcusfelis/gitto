-module(gitto_deps_gexf).
-export([build_xml/1]).
-include_lib("gitto/src/gitto.hrl").
-compile([export_all, {parse_transform, lager_transform}]).


build_xml(Reps) ->
%   lager:info("Reps: ~p", [Reps]),
    lager:info("Number of all repositories: ~p", [length(Reps)]),

    SrcReps = drop_forks(Reps),
    lager:info("Number of source repositories: ~p", [length(SrcReps)]),

    SrcNameSet = sets:from_list(lists2:keys(#gh_repository.fullname, SrcReps)),

    %% Same length of the list, but lower the count dependencies for each rep.
    SrcReps2 = validate_dependencies(SrcReps, SrcNameSet),

    FullName2Id = dict:from_list([{FullName, Id}
                                  || #gh_repository{id = Id, fullname = FullName}
                                     <- SrcReps2]),

    %% Fullname to fullname.
    ParentChildPairs = [{FullName, Dep}
                        || #gh_repository{fullname = FullName,
                                          src_dependencies = Deps} <- SrcReps2,
                           Dep <- Deps],

    Nodes = [rep_node(Rep) || Rep <- SrcReps2],
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


rep_node(#gh_repository{id = Id, fullname = FullName}) ->
    gexf:set_label(print_fullname(FullName), gexf:node(Id)).


dep_edge(FullName2Id, EdgeId, FullName, DepFullName) ->
    RepId = fun(RepName) -> dict:fetch(RepName, FullName2Id) end,
    gexf:edge(EdgeId, RepId(FullName), RepId(DepFullName)).


print_fullname(Name) ->
    io_lib:format("~p", [Name]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_xml_test() ->
    lager:start(),
    FileName = filename:join(code:priv_dir(gitto), "reps.bin"),
    {ok, RepsBin} = file:read_file(FileName),
    Xml = build_xml(enumerate(binary_to_term(RepsBin))),
    io:format(user, "Xml: ~p", [Xml]),
    gexf:to_string(Xml),
    ok.


enumerate(L) ->
    enumerate(L, 1).

enumerate([H|T], X) ->
    [H#gh_repository{id = X}|enumerate(T)];
enumerate([], _X) ->
    [].


-endif.

