-module(gitto_github).
-export([run_import/0]).
-include_lib("gitto/src/gitto.hrl").


run_import() ->
    Con = create_gh_connector(),
    RepList = gh_lib:extract_all(Con, "erlang", "erlang", 1),
    F =
        fun(X) ->
            query_and_fill_app_structore(Con, 
                                         fill_gh_repository(#gh_repository{}, X))
        end,
    plists:map(F, RepList, {processes, 10}).


fill_gh_repository(Rec, [{K, V}|PL]) ->
    Rec2 =
        case K of
            <<"name">> ->
                Rec#gh_repository{name = V};
            <<"description">> ->
                Rec#gh_repository{description = V};
            <<"owner">> ->
                Rec#gh_repository{owner = V};
            <<"created">> ->
                Rec#gh_repository{created = datetime_to_timestamp(V)};
            <<"pushed">> ->
                Rec#gh_repository{pushed = datetime_to_timestamp(V)};
            <<"forks">> ->
                Rec#gh_repository{forks = V};
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


query_and_fill_app_structore(Server,
                             Rec = #gh_repository{name = RepName, 
                                                  owner = RepOwner}) ->
    Props = gh_lib:extract_application_structure(Server, RepOwner, RepName),
    Rec#gh_repository{
        template = detect_template(Props),
        
        has_rebar_config = lists:member(has_rebar_config, Props),
        has_c_src_dir    = lists:member(has_c_src_dir, Props),
        has_rel_dir      = lists:member(has_rel_dir, Props),
        has_priv_dir     = lists:member(has_priv_dir, Props),
        has_test_dir     = lists:member(has_test_dir, Props)
        }.


datetime_to_timestamp(DT) ->
    calendar:datetime_to_gregorian_seconds(iso8601:parse(DT)).


create_gh_connector() ->                                                      
    {ok, CacheServer} = riakc_pb_socket:start_link("127.0.0.1", 8087),   
    {ok, Con} = gh_api:new([{cache_server, CacheServer},
                            {user, "secret"},
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

       


-ifdef(TEST).                           
-include_lib("eunit/include/eunit.hrl").

date_convert_test_() ->
    [?_assertEqual(iso8601:parse(<<"2011-11-01T11:35:33-07:00">>),
                   {{2011,11,1},{18,35,33}}),
     ?_assertEqual(calendar:datetime_to_gregorian_seconds({{2011,11,1},{18,35,33}}),
                   63487391733)
    ].

-endif.
