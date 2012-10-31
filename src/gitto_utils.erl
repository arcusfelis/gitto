-module(gitto_utils).
-export([timestamp/0, consult_string/1, parse_commit_hashes/1,
         parse_commit_hashes_and_filenames/1]).
-compile({parse_transform, lager_transform}).


%% Now timestamp in seconds.
timestamp() ->
    {Mega,Sec,_Micro} = os:timestamp(),
    Mega*1000000+Sec.


%% --------------------------------------------------------------------
%% Consult string
%% --------------------------------------------------------------------

consult_string(Bin) when is_binary(Bin) ->
    consult_string(binary_to_list(Bin));
consult_string(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    Groups = token_groups(fun is_dot/1, Tokens),
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


parse_commit_hashes_and_filenames(Str) ->
    Lines = lines(Str),
    lager:info("Lines: ~p~n", [Lines]),
    parse_commit_hashes_and_filenames_lines(Lines).

parse_commit_hashes_and_filenames_lines([<<Hash:40/binary>>, <<>>|T]) ->
    IsFile = fun(<<$:, _/binary>>) -> true; (_) -> false end,
    {Files, T2} = lists:splitwith(IsFile, T),
    FileNames = [extract_filename(X) || X <- Files],
    [{Hash, FileNames} | parse_commit_hashes_and_filenames_lines(T2)];
parse_commit_hashes_and_filenames_lines([<<>>]) ->
    [].




lines(Str) ->
    binary:split(Str, line_terminator(), [global]).

line_terminator() ->
    iolist_to_binary(io_lib:format("~n", [])).
    

%% In: `:000000 100644 0000000... f2caaf3... A  rebar.config'.
%% Out: `rebar.config'.
extract_filename(<<_:39/binary, Name/binary>>) -> Name.


-ifdef(TEST).                           
-include_lib("eunit/include/eunit.hrl").

test_data() ->
% git whatchanged --format="%H" -- 'rebar.config*'
<<"650c7db44349a18eb0794af33cc30837c0d1c536

:100644 100644 f2caaf3... 22d6d5a... M\trebar.config
0b2417bae993f7ba51595e71d83dbd2e1c911484

:000000 100644 0000000... f2caaf3... A\trebar.config
:000000 100644 0000000... fffeerr... A\trebar.config.example
">>.

parse_commit_hashes_test_() ->                                     
    Src = test_data(),

    Rec1 = <<"650c7db44349a18eb0794af33cc30837c0d1c536">>,
    Rec2 = <<"0b2417bae993f7ba51595e71d83dbd2e1c911484">>,

    ExpectedResult = [Rec1, Rec2],
    Result = ?MODULE:parse_commit_hashes(Src),
    [?_assertEqual(Result, ExpectedResult)]. 

parse_commit_hashes_and_filenames_test_() ->
    Src = test_data(),

    Rec1 = {<<"650c7db44349a18eb0794af33cc30837c0d1c536">>, 
            [<<"rebar.config">>]},
    Rec2 = {<<"0b2417bae993f7ba51595e71d83dbd2e1c911484">>, 
            [<<"rebar.config">>, <<"rebar.config.example">>]},

    ExpectedResult = [Rec1, Rec2],
    Result = ?MODULE:parse_commit_hashes_and_filenames(Src),
    [?_assertEqual(Result, ExpectedResult)]. 

-endif.

%% --------------------------------------------------------------------
%% end of parse commits 
%% --------------------------------------------------------------------
