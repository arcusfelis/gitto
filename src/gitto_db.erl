-module(gitto_db).
-export([up/0, down/0]).
-export([ lookup/2
        , read/2
        , write/1
        , update_with/3
        , remove/2
        , record_to_id/1
        , match_object/1

        , select/1
        , select1/1
        , transaction/1]).

-include_lib("gitto/src/gitto.hrl").


%% -----------------------------------------------------------------------
%% Deploy
%% -----------------------------------------------------------------------

up() ->
    stopped = mnesia:stop(),
    case mnesia:create_schema([node()]) of
        {error,{_Node,{already_exists,_Node}}} ->
            ok = mnesia:start(),
            ok;
        ok -> 
            error_logger:info_msg("Create tables.~n", []),
            
            ok = mnesia:start(),

            mnesia:create_table(g_project,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_project)}
                ]),

            mnesia:create_table(g_repository,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_repository)}
                ]),

            mnesia:create_table(g_address,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_address)}
                ]),

            mnesia:create_table(g_person,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_person)}
                ]),

            mnesia:create_table(g_revision,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_revision)}
                ,{index, [commit_hash]}
                ]),

            mnesia:create_table(g_dependency,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_dependency)}
                ]),

            mnesia:create_table(g_repository_x_revision,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_repository_x_revision)}
                ,{index, [revision]}
                ]),

            mnesia:create_table(g_tag,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_tag)}
                ,{index, [name]}
                ]),

            mnesia:create_table(g_revision_date_index,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_revision_date_index)}
                ]),

            mnesia:create_table(g_application,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_application)}
                ]),

            Res = mnesia:create_table(g_counter,
                [{type, set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, g_counter)}
                ]),

            case Res of
                ok -> 
                    [mnesia:write(T, #g_counter{table = T, last_id = 0})
                     || T <- tables(), T =/= g_counter];
                _Other ->
                    %% Table was already created.
                    ok
            end
    end,

    ok = mnesia:wait_for_tables(tables(), 3000),
    ok.


down() ->
    [mnesia:delete_table(X) || X <- tables()],
    ok.


tables() ->
    [g_project, g_repository, g_address, g_person, 
     g_revision, g_dependency, g_tag, g_repository_x_revision,
     g_revision_date_index, g_application, g_counter].



%% -----------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------

next_id(Tab) ->
    PrevN = mnesia:dirty_update_counter(g_counter, Tab, #g_counter.last_id),
    PrevN + 1.


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


update_with(F, Tab, Id) when is_function(F, 1), Id =/= undefined ->
    TransF = fun() -> 
            [begin 
                NewRec = F(Rec),
                ok = mnesia:write(NewRec),
                NewRec
             end || Rec <- mnesia:read(Tab, Id)]
        end,
    case mnesia:transaction(TransF) of
    {atomic, Result} ->
        Result
    end.


read(Tab, Id) ->
    F = fun() -> mnesia:read(Tab, Id) end,
    case mnesia:transaction(F) of
    {atomic, Result} ->
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


transaction(F)->
    %% to prevent against nested transactions
    %% to ensure it also works whether table
    %% is fragmented or not, we will use
    %% mnesia:activity/4

    case mnesia:is_transaction() of
        false ->
            {atomic, X} = mnesia:transaction(F),
            X;
        true -> F()
    end.


select1(Q) ->
    case select(Q) of
        []  -> undefined;
        [X] -> X
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


