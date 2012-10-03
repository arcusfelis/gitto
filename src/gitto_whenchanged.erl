-module(gitto_whenchanged).
-export([parse_commits/1]).



parse_commits(Str) ->
    Lines = lines(Str),
    [Hash || <<Hash:40/binary>> <- Lines, binary:first(Hash) =/= $:].


lines(Str) ->
    binary:split(Str, line_terminator(), [global]).

line_terminator() ->
    iolist_to_binary(io_lib:format("~n", [])).
    


-ifdef(TEST).                           
-include_lib("eunit/include/eunit.hrl").

parse_commits_test_() ->                                     
Src = 
% git whatchanged --format="%H" rebar.config
<<"650c7db44349a18eb0794af33cc30837c0d1c536

:100644 100644 f2caaf3... 22d6d5a... M  rebar.config
0b2417bae993f7ba51595e71d83dbd2e1c911484

:000000 100644 0000000... f2caaf3... A  rebar.config">>,

    Rec1 = <<"650c7db44349a18eb0794af33cc30837c0d1c536">>,
    Rec2 = <<"0b2417bae993f7ba51595e71d83dbd2e1c911484">>,

    ExpectedResult = [Rec1, Rec2],
    Result = ?MODULE:parse_commits(Src),
    [?_assertEqual(Result, ExpectedResult)]. 

-endif.
