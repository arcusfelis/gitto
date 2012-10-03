-module(gitto_log).
-export([format/0, parse_commits/1]).


format() ->
    "%H%n"      %% commit hash
    "%P%n"      %% parent hashes

    "%an%n"     %% author name
    "%ae%n"     %% author email
    "%at%n"     %% author date, UNIX timestamp

    "%cn%n"     %% committer name
    "%ce%n"     %% committer email
    "%ct%n"     %% committer date, UNIX timestamp

    "%s%n%n%n"  %% subject
    "%B%n%n%n". %% raw body (unwrapped subject and body)


parse_commits(Str) ->
    Lines = lines(Str),
    parse_commit_lines(Lines, []).


parse_commit_lines([<<>> | Lines], Acc) ->
    parse_commit_lines(Lines, Acc);

parse_commit_lines([CommitHash, ParentHashes, 
                    AuthorName, AuthorEmail, AuthorTime,
                    CommitterName, CommitterEmail, CommitterTime | Lines], Acc) ->
    {Subject, Lines2} = parse_body(Lines, []),
    {Body,    Lines3} = parse_body(Lines2, []),
    %% El is proplist.
    El = [ {commit_hash, CommitHash} 
         , {parent_hashes, split_hashes(ParentHashes)}

         , {author_name,    AuthorName}
         , {author_email,   AuthorEmail}
         , {author_date,    format_timestamp(AuthorTime)}

         , {committer_name,     CommitterName}
         , {committer_email,    CommitterEmail}
         , {committer_date,     format_timestamp(CommitterTime)}

         , {subject, join_lines(Subject)}
         , {body,    join_lines(Body)}
         ],
    parse_commit_lines(Lines3, [El | Acc]);

parse_commit_lines([], Acc) ->
    lists:reverse(Acc).


parse_body([<<>>, <<>> | Lines], Acc) ->
    {lists:reverse(Acc), Lines};

parse_body([Line|Lines], Acc) ->
    parse_body(Lines, [Line|Acc]).
    

split_hashes(Str) ->
    binary:split(Str, <<" ">>, [global]).

lines(Str) ->
    binary:split(Str, line_terminator(), [global]).


join_lines(Lines) ->
    binary2:join(Lines, <<"\r\n">>).


line_terminator() ->
    iolist_to_binary(io_lib:format("~n", [])).


format_timestamp(X) ->
    list_to_integer(binary_to_list(X)).

-ifdef(TEST).                           
-include_lib("eunit/include/eunit.hrl").

parse_commits_test_() ->                                     
Src = 
<<"650c7db44349a18eb0794af33cc30837c0d1c536
0b2417bae993f7ba51595e71d83dbd2e1c911484
Uvarov Michael
freeakk@gmail.com
1349067700
Uvarov Michael
freeakk@gmail.com
1349067700
Add other files.


Add other files.




0b2417bae993f7ba51595e71d83dbd2e1c911484

Uvarov Michael
freeakk@gmail.com
1348931051
Uvarov Michael
freeakk@gmail.com
1348931051
First commit.


First commit.




">>,

    Rec1 = 
    [{commit_hash,<<"650c7db44349a18eb0794af33cc30837c0d1c536">>}
    ,{parent_hashes,[<<"0b2417bae993f7ba51595e71d83dbd2e1c911484">>]}
    ,{author_name,<<"Uvarov Michael">>}
    ,{author_email,<<"freeakk@gmail.com">>}
    ,{author_date,1349067700}
    ,{committer_name,<<"Uvarov Michael">>}
    ,{committer_email,<<"freeakk@gmail.com">>}
    ,{committer_date,1349067700}
    ,{subject,<<"Add other files.">>}
    ,{body,<<"Add other files.">>}],

    Rec2 = 
    [{commit_hash,<<"0b2417bae993f7ba51595e71d83dbd2e1c911484">>}
    ,{parent_hashes,[<<>>]}
    ,{author_name,<<"Uvarov Michael">>}
    ,{author_email,<<"freeakk@gmail.com">>}
    ,{author_date,1348931051}
    ,{committer_name,<<"Uvarov Michael">>}
    ,{committer_email,<<"freeakk@gmail.com">>}
    ,{committer_date,1348931051}
    ,{subject,<<"First commit.">>}
    ,{body,<<"First commit.">>}],

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
