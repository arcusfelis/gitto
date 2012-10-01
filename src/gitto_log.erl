-module(gitto_log).
-export([parse_commits/1]).



parse_commits(Str) ->
    Lines = lines(Str),
    parse_commit_lines(Lines, []).


parse_commit_lines([<<>> | Lines], Acc) ->
    %% Skip an empty string between two records.
    parse_commit_lines(Lines, Acc);

parse_commit_lines([HashLine, AuthorLine, DateLine, <<>> | Lines], Acc) ->
    <<"commit ", Hash/binary>> = HashLine,
    <<"Author: ", Author/binary>> = AuthorLine,
    <<"Date:   ", Date/binary>> = DateLine,
    {Body, Lines2} = parse_body(Lines, []),
    Header = body_to_header(Body),
    {DateTimeStamp, _DateTimeZone} = parse_date(Date),
    %% El is an ordset.
    El = [ {author, Author} 
         , {body, join_lines(Body)}
         , {commit, Hash}
         , {date_timestamp, DateTimeStamp}
         , {header, join_lines(Header)}],
    parse_commit_lines(Lines2, [El | Acc]);

parse_commit_lines([], Acc) ->
    lists:reverse(Acc).


parse_body([<<"    ", Mess/binary>> | Lines], Acc) ->
    io:format("Message string was parsed: ~ts~n", [Mess]),
    parse_body(Lines, [Mess|Acc]);

parse_body(Lines, Acc) ->
    {lists:reverse(Acc), Lines}.


body_to_header(Lines) ->
    lists:takewhile(fun(X) -> X =/= <<>> end, Lines).
    


lines(Str) ->
    binary:split(Str, line_terminator(), [global]).


join_lines(Lines) ->
    binary2:join(Lines, <<"\r\n">>).


line_terminator() ->
    iolist_to_binary(io_lib:format("~n", [])).


-spec parse_date(Date) -> {TimeStamp, TimeZone} when
    Date :: binary(),
    TimeStamp :: integer(),
    TimeZone :: binary().

parse_date(Bin) ->
    [TS, TZ] = binary:split(Bin, <<" ">>),
    {list_to_integer(binary_to_list(TS)), TZ}.
    


-ifdef(TEST).                           
-include_lib("eunit/include/eunit.hrl").

parse_commits_test_() ->                                     
Src = 
% git log --pretty=medium  --date=raw
<<"commit 08d2879b56b604b41e3d40fe4fa721465bcb36e5
Author: Uvarov Michael <freeakk@gmail.com>
Date:   1348992863 +0400

    Header
    Mess1
    Mess2

commit 97cc7209ee81a660eb71bd112d9c19776a88b900
Author: Uvarov Michael <freeakk@gmail.com>
Date:   1348986807 +0400

    Header
    
    Mess1
    Mess2
    
    Mess3">>,

    Rec1 = 
    [ {author, <<"Uvarov Michael <freeakk@gmail.com>">>}
    , {body, <<"Header\r\nMess1\r\nMess2">>}
    , {commit, <<"08d2879b56b604b41e3d40fe4fa721465bcb36e5">>}
    , {date_timestamp, 1348992863}
    , {header, <<"Header\r\nMess1\r\nMess2">>}],

    Rec2 = 
    [ {author, <<"Uvarov Michael <freeakk@gmail.com>">>}
    , {body, <<"Header\r\n\r\nMess1\r\nMess2\r\n\r\nMess3">>}
    , {commit, <<"97cc7209ee81a660eb71bd112d9c19776a88b900">>}
    , {date_timestamp, 1348986807}
    , {header, <<"Header">>}],

    ExpectedResult = [Rec1, Rec2],
    Result = ?MODULE:parse_commits(Src),
    [assertListGenerator(lists:flatten(Result), lists:flatten(ExpectedResult))
    ,?_assertEqual(Result, ExpectedResult)]. 


assertListGenerator([H1|T1], [H2|T2]) ->
    {generator, fun() -> [?_assertEqual(H1, H2) | assertListGenerator(T1, T2)] end};
assertListGenerator([], []) ->
    [];
assertListGenerator(L1, L2) ->
    [?_assertEqual(L1, L2)].

-endif.
