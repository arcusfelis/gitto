-module(gitto_import).
-export([lhttpc_start/0]).
-export([single_file/1, single_file/2]).
-export([github_test/0]).


lhttpc_start() ->
    ssl:start(),
    lhttpc:start(),
    ok.


%% @doc Request a single file. 
%% This function returns a HOF, that can be called with a basename of a file.
single_file({git, URL, Branch}) ->
    single_file(URL, Branch).


%% git archive --remote=git://github.com/erlang-unicode/ux.git HEAD rebar.config | tar -x
single_file("git://github.com/" ++ URL, Branch) ->
    single_file_from_github(URL, Branch);
single_file("https://github.com/" ++ URL, Branch) ->
    single_file_from_github(URL, Branch).


%% single_file_from_github("erlang-unicode/ux.git", "master").
single_file_from_github(URL, Branch) ->
    URL2 = maybe_remove_suffix(".git", URL),
    URL3 = "https://raw.github.com/" ++ URL2 ++ "/" ++ Branch ++ "/",
    fun(Filename) ->
        Result = lhttpc:request(URL3 ++ Filename, get, [], 5000),
        case Result of
            {ok,{{200,"OK"}, _Headers, Body}} -> {ok, Body};
                Error -> {error, {http_error, Error}}
        end
        end.


%% @doc Remove the suffix `Suffix' from the string `Str'.
%% Return `Str', if the `Suffix' is not the end of the `Str'.
-spec maybe_remove_suffix(Suffix, Str) -> Str when
    Suffix :: list(),
    Str :: list().

maybe_remove_suffix(Suffix, Suffix) ->
    [];
maybe_remove_suffix(Suffix, [H|T]) ->
    [H|maybe_remove_suffix(Suffix, T)];
maybe_remove_suffix(_Suffix, []) ->
    [].


github_test() ->
    gitto_import:lhttpc_start(),
    read_rebar_config({git, "git://github.com/erlang-unicode/ux.git", "HEAD"}).


%% {git, "git://github.com/erlang-unicode/ux.git", "HEAD"}
read_rebar_config(RepAddress) ->
    Fn = gitto_import:single_file(RepAddress),
    case Fn("rebar.config") of
        {ok, BodyBin} -> 
            Settings = consult_string(BodyBin),
            Deps = proplists:get_value(deps, Settings),
            [handle_dependency(Dep) || Dep <- Deps];
        Error ->
            Error
    end.

handle_dependency({Name, _Pattern, RepAddress}) ->
    {Name, read_rebar_config(RepAddress)}.



%% --------------------------------------------------------------------
%% Consult string
%% --------------------------------------------------------------------

consult_string(Bin) when is_binary(Bin) ->
    consult_string(binary_to_list(Bin));
consult_string(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    Groups = token_groups(fun is_dot/1, Tokens),
%   io:format("~p", [Groups]),
    [ok_value(erl_parse:parse_term(Group)) || Group <- Groups].

is_dot({dot, _}) -> true;
is_dot(_) -> false.

ok_value({ok, X}) -> X.


%% @doc Split the `List' with `DelimFn'.
%% Delimeter is inluded.
-spec token_groups(DelimFn, List) -> [List] when
    List :: [El],
    El :: term(),
    DelimFn :: fun((El) -> boolean()).

token_groups(DelimFn, List) ->
    token_groups(DelimFn, List, [], []).


token_groups(DelimFn, [H|T], Acc, Groups) ->
    case DelimFn(H) of
        true -> 
            token_groups(DelimFn, T, [], [lists:reverse(Acc, [H]) | Groups]);
        false -> 
            token_groups(DelimFn, T, [H|Acc], Groups)
    end;
token_groups(_DelimFn, [], [], Groups) ->
    lists:reverse(Groups);
token_groups(_DelimFn, [], Acc, Groups) ->
    lists:reverse(Groups, [lists:reverse(Acc)]).


%% --------------------------------------------------------------------
%% End of consult string 
%% --------------------------------------------------------------------
