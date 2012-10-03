-module(gitto_utils).
-export([consult_string/1]).


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



