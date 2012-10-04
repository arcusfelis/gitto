-module(gitto_exec).
-export([download/2]).



download(Cfg, Rep) ->
    Addrs = gitto_db:select(gitto_store:repository_addresses(Rep)),
    LocalPath = local_repository_path(Cfg, Rep),
    download_one_of(Addrs, LocalPath).


local_repository_path(Cfg, Rep) ->
    gitto_config:get_value(bare_reps_dir, Cfg)
        ++ gitto_store:repository_literal_id(Rep).



-spec download_one_of([Addr], LocalPath) -> Addr | undefined
    when
    LocalPath :: filename:path(),
    Addr :: gitto_type:address().


download_one_of([], _LocalPath) ->
    error_logger:error_msg("Downloading error: no addresses.~n", []),
    undefined;

download_one_of([Addr|Addrs], LocalPath) ->
    try
        gitto_rep:clone(gitto_store:address_to_url(Addr), LocalPath),
        Addr
        catch Type:Error ->
        error_logger:error_msg("Downloading error ~p:~p from ~p.~n",
                               [Type, Error, Addr]),
        download_one_of(Addrs, LocalPath)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

download_test() ->
    gitto_db:up(),
    
    Project = gitto_db:write(gitto_store:repository([])),
    io:format(user, "Project: ~p~n", [Project]),

    RepCon = [{project, gitto_store:to_id(Project)}],
    Rep = gitto_db:write(gitto_store:repository(RepCon)),

    ?MODULE:download(gitto_config:example(), Rep).

-endif.

