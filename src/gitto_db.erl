-module(gitto_db).
-export([up/0, down/0]).
-export([ lookup/2
        , write/1
        , remove/2
        , record_to_id/1
        , match_object/1

        , select/1]).



%% -----------------------------------------------------------------------
%% Type Definitions
%% -----------------------------------------------------------------------

-type person_id()     :: non_neg_integer().
-type revision_id()   :: non_neg_integer().
-type project_id()    :: non_neg_integer().
-type address_id()    :: non_neg_integer().
-type repository_id() :: non_neg_integer().

-type timestamp() :: integer().


%% The binary of length 40.
-type hash()      :: binary().


%% -----------------------------------------------------------------------
%% Record Definitions
%% -----------------------------------------------------------------------

-record(g_project, {
        id :: project_id(),
        name :: unicode:unicode_binary()
}).

-record(g_repository, {
        id :: repository_id(),
        project :: project_id()
}).

-record(g_address, {
    id :: address_id(),
    repository :: repository_id(),
    url,

    is_dead :: boolean() | undefined,
    last_try_date :: timestamp() | undefined,
    last_successful_connection_date :: timestamp() | undefined
}).

-record(g_person, {
        id      :: person_id(),
        name    :: unicode:unicode_binary(),
        email   :: binary()
}).

-record(g_revision, {
        id              :: revision_id(),
        repository      :: repository_id(),
        %% Is the first on the right on the "git log --graph?"
        is_first_parent :: boolean(),
        author          :: person_id(),
        committer       :: person_id(),
        author_date     :: timestamp(),
        committer_date  :: timestamp(),

        commit_hash     :: hash(),
        subject         :: unicode:unicode_binary(),
        body            :: unicode:unicode_binary(),

        parents   = []  :: [revision_id()],
        dependencies = [] :: [repository_id()]
}).


%% -----------------------------------------------------------------------
%% Deploy
%% -----------------------------------------------------------------------

up() ->
    mnesia:create_schema([node()]),
    mnesia:start(),

    mnesia:create_table(g_project,
        [ {type, ordered_set}, {disc_copies, [node()] },
             {attributes,
                record_info(fields, g_project)} ]),

    mnesia:create_table(g_repository,
        [ {type, ordered_set}, {disc_copies, [node()] },
             {attributes,
                record_info(fields, g_repository)} ]),

    mnesia:create_table(g_address,
        [ {type, ordered_set}, {disc_copies, [node()] },
             {attributes,
                record_info(fields, g_address)} ]),

    mnesia:create_table(g_person,
        [ {type, ordered_set}, {disc_copies, [node()] },
             {attributes,
                record_info(fields, g_person)} ]),

    mnesia:create_table(g_revision,
        [ {type, ordered_set}, {disc_copies, [node()] },
             {attributes,
                record_info(fields, g_revision)} ]),

    ok.

down() ->
    mnesia:delete_table(g_person),
    mnesia:delete_table(g_revision),
    mnesia:delete_table(g_project),
    mnesia:delete_table(g_repository),
    mnesia:delete_table(g_address),
    ok.



%% -----------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------

next_id(Tab) ->
    case mnesia:last(Tab) of
    '$end_of_table' -> 1;
    N when is_integer(N) -> N+1
    end.


write(Rec) ->
    Id = record_to_id(Rec),
    case Id of
        undefined ->
            insert(Rec);
        _Other ->
            update(Rec)
    end.


%% New record
insert(Rec) ->
    Table = erlang:element(1, Rec),
    F = fun() ->
        NextId = next_id(Table),
        Rec2 = erlang:setelement(2, Rec, NextId),
        mnesia:write(Rec2),
        Rec2
        end,
    case mnesia:is_transaction() of
        false ->
            {atomic, Rec3} = mnesia:transaction(F),
            Rec3;
        true ->
            F()
    end.


%% Update
update(Rec) ->
    case mnesia:is_transaction() of
        false ->
            F = fun() -> mnesia:write(Rec) end,
            {atomic, ok} = mnesia:transaction(F),
            Rec;
        true ->
            mnesia:write(Rec),
            Rec
    end.


lookup(Tab, Id) ->
    F = fun() -> mnesia:read(Tab, Id) end,
    case mnesia:transaction(F) of
    {atomic, [Result]} ->
        Result
    end.


remove(Table, Id) ->
    case mnesia:is_transaction() of
        false ->
            F = fun() -> mnesia:delete({Table, Id}), ok end,
            {atomic, ok} = mnesia:transaction(F),
            ok;
        true ->
            mnesia:delete({Table, Id}),
            ok
    end.



%% query list comprehenshions
select(Q)->
    %% to prevent against nested transactions
    %% to ensure it also works whether table
    %% is fragmented or not, we will use
    %% mnesia:activity/4

    case mnesia:is_transaction() of
        false ->
            F = fun(QH)-> qlc:e(QH) end,
            mnesia:activity(transaction,F,[Q],mnesia_frag);
        true -> qlc:e(Q)
    end.


match_object(Pattern) ->
    case mnesia:is_transaction() of
        false ->
            F = fun(P) -> mnesia:match_object(P) end,
            mnesia:activity(transaction,F,[Pattern],mnesia_frag);
        true ->
            mnesia:match_object(Pattern)
    end.


%% -----------------------------------------------------------------------
%% Private helpers
%% -----------------------------------------------------------------------

record_to_id(Rec) ->
    erlang:element(2, Rec).
