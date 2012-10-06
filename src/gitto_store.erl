-module(gitto_store).
-include_lib("gitto/src/gitto.hrl").
-compile({parse_transform, vodka}).
-include_lib("stdlib/include/qlc.hrl").

-export([to_id/1]).

-export([repository_addresses/1,
         repository_literal_id/1,
         repository/1,
         repository/2]).

-export([address_to_url/1,
         address/1,
         address/2]).

-export([project/1,
         project/2]).

-export([revision/1,
         revision/2]).

-export([improper_revision/1,
         improper_revision/2]).


to_id(Rec) ->
    erlang:element(2, Rec).

%% ------------------------------------------------------------------
%% Repository
%% ------------------------------------------------------------------

repository_addresses(#g_repository{id = RepId}) ->
    repository_addresses(RepId);

repository_addresses(RepId) when is_integer(RepId) ->
    qlc:q([X || X=#g_address{repository = RepIdI} 
                    <- mnesia:table(g_address), RepId =:= RepIdI]).


repository_literal_id(#g_repository{id = RepId}) ->
    repository_literal_id(RepId);

repository_literal_id(RepId) ->
    integer_to_list(RepId).


repository(PL) ->
    repository(PL, #g_repository{}).

repository(PL, Rec) ->
    proplist_to_record(fun set_repository_field/3, PL, Rec).

set_repository_field(K, V, A) ->
    A#g_repository{K = V}.


%% ------------------------------------------------------------------
%% Address
%% ------------------------------------------------------------------

address_to_url(#g_address{url = Url}) ->
    Url.


address(PL) ->
    address(PL, #g_address{}).

address(PL, Rec) ->
    proplist_to_record(fun set_address_field/3, PL, Rec).

set_address_field(K, V, A) ->
    A#g_address{K = V}.


proplist_to_record(F, [{K,V}|PL], Rec) ->
    proplist_to_record(F, PL, F(K, V, Rec));

proplist_to_record(_F, [], Rec) ->
    Rec.
    



%% ------------------------------------------------------------------
%% Project
%% ------------------------------------------------------------------

project(PL) ->
    project(PL, #g_project{}).

project(PL, Rec) ->
    proplist_to_record(fun set_project_field/3, PL, Rec).

set_project_field(K, V, A) ->
    A#g_project{K = V}.



%% ------------------------------------------------------------------
%% Revision
%% ------------------------------------------------------------------

%% Commits (revisions) are added in the committer date order.


revision(PL) ->
    revision(PL, #g_revision{}).


revision(ImPropRec = #g_improper_revision{}, PropRec) ->
    PL = impoper_revision_to_proper_proplist(ImPropRec),
    revision(PL, PropRec);
    
revision(PL, Rec) ->
    proplist_to_record(fun set_revision_field/3, PL, Rec).

set_revision_field(K, V, A) ->
    A#g_revision{K = V}.


-spec revision_ids(RepId, Ids, Hashes) -> Ids | undefined when
    RepId   :: gitto_type:repository_id(),
    Ids     :: [gitto_type:revision_id()],
    Hashes  :: [gitto_type:hash()].

revision_ids(_, undefined, undefined) ->
    undefined;

revision_ids(_, Ids, undefined) ->
    Ids;

revision_ids(_, undefined, []) ->
    [];

revision_ids(RepId, undefined, Hashes) when RepId =/= undefined ->
    Q = qlc:q([X || X=#g_revision{commit_hash = H, repository = R}
                         <- mnesia:table(g_revision),
                    Hash <- Hashes,
                    H =:= Hash,
                    R =:= RepId]),
    RevIds = qlc:e(Q),
    %% One of the hashes is not in the database.
    true = length(RevIds) =:= length(Hashes),
    RevIds.


improper_revision(PL) ->
    improper_revision(PL, #g_improper_revision{}).

improper_revision(PL, Rec) ->
    proplist_to_record(fun set_improper_revision_field/3, PL, Rec).

set_improper_revision_field(K, V, A) ->
    A#g_improper_revision{K = V}.

impoper_revision_to_proper_proplist(X = #g_improper_revision{}) ->
    [{id,               X#g_improper_revision.id}
    ,{repository,       X#g_improper_revision.repository}
    ,{is_first_parent,  X#g_improper_revision.is_first_parent}
    ,{author,           person_id(X#g_improper_revision.author,
                                  X#g_improper_revision.author_name,
                                  X#g_improper_revision.author_email)}
    ,{committer,        person_id(X#g_improper_revision.committer,
                                  X#g_improper_revision.committer_name,
                                  X#g_improper_revision.committer_email)}
    ,{author_date,      X#g_improper_revision.author_date}
    ,{committer_date,   X#g_improper_revision.committer_date}

    ,{commit_hash,      X#g_improper_revision.commit_hash}
    ,{subject,          X#g_improper_revision.subject}
    ,{body,             X#g_improper_revision.body}

    ,{parents,          revision_ids(X#g_improper_revision.repository,
                                     X#g_improper_revision.parents,
                                     X#g_improper_revision.parent_hashes)}
    ,{dependencies,     X#g_improper_revision.dependencies}
    ].


%% ------------------------------------------------------------------
%% Person
%% ------------------------------------------------------------------

create_person(Name, Email) ->
    gitto_db:write(#g_person{name = Name, email = Email}).


-spec person_id(Id, Name, Email) -> Id | undefined when
    Id      :: gitto_type:person_id(),
    Name    :: unicode:unicode_binary(),
    Email   :: binary().

person_id(undefined, undefined, undefined) ->
    undefined;

person_id(undefined, Name, Email) ->
    Q = qlc:q([X || X=#g_person{name = NameI, email = EmailI} 
                        <- mnesia:table(g_address), 
                    Name =:= NameI,
                    Email =:= EmailI]),
    case gitto_db:select(Q) of
        []   -> to_id(create_person(Name, Email));
        [Id] -> Id
    end;

person_id(Id, undefined, undefined) ->
    Id.
