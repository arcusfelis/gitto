-module(gitto_utils).
-export([timestamp/0, consult_string/1, parse_commit_hashes/1]).


%% Now timestamp in seconds.
timestamp() ->
    {Mega,Sec,_Micro} = erlang:now(),
    Mega*1000000+Sec.


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



%% --------------------------------------------------------------------
%% Parse commits 
%% --------------------------------------------------------------------

parse_commit_hashes(Str) ->
    Lines = lines(Str),
    [Hash || <<Hash:40/binary>> <- Lines, binary:first(Hash) =/= $:].


lines(Str) ->
    binary:split(Str, line_terminator(), [global]).

line_terminator() ->
    iolist_to_binary(io_lib:format("~n", [])).
    


-ifdef(TEST).                           
-include_lib("eunit/include/eunit.hrl").

parse_commit_hashes_test_() ->                                     
Src = 
% git whatchanged --format="%H" rebar.config
<<"650c7db44349a18eb0794af33cc30837c0d1c536

:100644 100644 f2caaf3... 22d6d5a... M  rebar.config
0b2417bae993f7ba51595e71d83dbd2e1c911484

:000000 100644 0000000... f2caaf3... A  rebar.config">>,

    Rec1 = <<"650c7db44349a18eb0794af33cc30837c0d1c536">>,
    Rec2 = <<"0b2417bae993f7ba51595e71d83dbd2e1c911484">>,

    ExpectedResult = [Rec1, Rec2],
    Result = ?MODULE:parse_commit_hashes(Src),
    [?_assertEqual(Result, ExpectedResult)]. 

-endif.

%% --------------------------------------------------------------------
%% end of parse commits 
%% --------------------------------------------------------------------
