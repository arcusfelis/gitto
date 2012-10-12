-module(gitto_store).
-include_lib("gitto/src/gitto.hrl").
-compile({parse_transform, vodka}).
-compile({parse_transform, arak}).
-include_lib("stdlib/include/qlc.hrl").

-export([to_id/1,
         table/1]).

-export([repository_addresses/1,
         repository_literal_id/1,
         repository/1,
         repository/2]).

-export([address_to_url/1,
         address/1,
         address/2,
         update_trying_date/1,
         update_downloading_date/1
        ]).

-export([project/1,
         project/2]).

-export([revision/1,
         revision/2,
         revision_hash/1,
         lookup_revision/1]).

-export([improper_revision/1,
         improper_revision/2]).

-export([dependency/2]).

-export([revision_date_index/2,
         repository_x_revision/3]).

to_id(Rec) ->
    erlang:element(2, Rec).

maybe_to_id(undefined) -> undefined;
maybe_to_id(Rec)       -> to_id(Rec).

table(Rec) ->
    erlang:element(1, Rec).



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

create_repository(ProjectId) ->
    gitto_db:write(#g_repository{project = ProjectId}).


%% ------------------------------------------------------------------
%% Address
%% ------------------------------------------------------------------


%% @doc Lookup or create.
address_to_url(#g_address{url = Url}) ->
    Url.

lookup_address(Url) ->
    Q = qlc:q([X || X=#g_address{} <- mnesia:table(g_address), 
                    Url =:= X#g_address.url]),
    gitto_db:select1(Q).

create_address(Url, RepId) when is_integer(RepId) ->
    gitto_db:write(#g_address{url = Url, repository = RepId}).

    

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
    
update_downloading_date(#g_address{id = Id}) ->
    Now = gitto_utils:timestamp(),
    [_] = gitto_db:update_with(fun(X) -> 
                X#g_address{
                    last_successful_connection_date = Now,
                    last_try_date = Now
                }
        end, g_address, Id),
    [_] = gitto_db:update_with(fun(X) -> 
                X#g_repository{
                    updated = Now
                }
        end, g_repository, Id),
    ok.


update_trying_date(#g_address{id = Id}) ->
    Now = gitto_utils:timestamp(),
    [_] = gitto_db:update_with(fun(X) -> 
                X#g_address{
                    last_try_date = Now
                }
        end, g_address, Id),
    ok.



%% ------------------------------------------------------------------
%% Project
%% ------------------------------------------------------------------

project(PL) ->
    project(PL, #g_project{}).

project(PL, Rec) ->
    proplist_to_record(fun set_project_field/3, PL, Rec).

set_project_field(K, V, A) ->
    A#g_project{K = V}.

create_project(Name) ->
    gitto_db:write(#g_project{name = Name}).


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


revision_hash(#g_revision{commit_hash = Hash}) -> Hash.


-spec revision_ids(Ids, Hashes) -> Ids | undefined when
    Ids     :: [gitto_type:revision_id()],
    Hashes  :: [gitto_type:hash()].

revision_ids(undefined, undefined) ->
    undefined;

revision_ids(Ids, undefined) ->
    Ids;

revision_ids(undefined, []) ->
    [];

revision_ids(undefined, Hashes) ->
    [revision_id(undefined, Hash) || Hash <- Hashes].


revision_id(undefined, undefined) ->
    undefined;

revision_id(undefined, Hash) ->
    Q = qlc:q([X.id || X = #g_revision{} <- mnesia:table(g_revision), 
                       Hash =:= X.commit_hash]),
    case gitto_db:select(Q) of
        []   -> to_id(create_revision(Hash));
        [Id] -> Id
    end;

revision_id(Id, undefined) ->
    Id.

partical_revision_id(Id, Hash) ->
    revision_id(Id, Hash).

lookup_revision(Hash) ->
    Q = qlc:q([X || X = #g_revision{} <- mnesia:table(g_revision), 
                       Hash =:= X.commit_hash]),
    gitto_db:select1(Q).
    

create_revision(Hash) ->
    gitto_db:write(#g_revision{commit_hash = Hash}).


improper_revision(PL) ->
    improper_revision(PL, #g_improper_revision{}).

improper_revision(PL, Rec) ->
    proplist_to_record(fun set_improper_revision_field/3, PL, Rec).

set_improper_revision_field(K, V, A) ->
    A#g_improper_revision{K = V}.

impoper_revision_to_proper_proplist(X = #g_improper_revision{}) ->
    [{id,               revision_id(X#g_improper_revision.id,
                                    X#g_improper_revision.commit_hash)}
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

    ,{parents,          revision_ids(X#g_improper_revision.parents,
                                     X#g_improper_revision.parent_hashes)}
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

%% Find or create.
person_id(undefined, Name, Email) ->
    Q = qlc:q([X.id || X=#g_person{name = NameI, email = EmailI} 
                            <- mnesia:table(g_address), 
                    Name =:= NameI,
                    Email =:= EmailI]),
    case gitto_db:select(Q) of
        []   -> to_id(create_person(Name, Email));
        [Id] -> Id
    end;

person_id(Id, undefined, undefined) ->
    Id.


%% ------------------------------------------------------------------
%% Dependency
%% ------------------------------------------------------------------

%% @doc Decode an element of rebar's deps directive.
dependency({Name,Vsn,{git,Url,RevDesc}} = Data, RcptRev = #g_revision{}) ->
    {DonorAddr, DonorRep, DonorProject} =
        case lookup_address(Url) of
            undefined -> 
                P = create_project(Name),
                R = create_repository(to_id(P)),
                A = create_address(Url, to_id(R)),
                {A, R, P};
            A = #g_address{} ->
                #g_repository{} =
                R = gitto_db:lookup(g_repository, A.repository),
                P = gitto_db:lookup(g_project,  R.project),
                {A, R, P}
        end,
    DonorRev =
        case lookup_dependency(RcptRev, DonorRep) of
            %% Not yet created.
            undefined -> 
                DonorRevId = calculate_revision(RevDesc, DonorRep, 
                                                RcptRev.author_date),
                lookup_dependency(to_id(RcptRev), DonorRevId);
            Dep -> Dep
        end,
    #g_dependency{
        donor = maybe_to_id(DonorRev),
        recipient = to_id(RcptRev),
        raw_data = Data,
        name = Name,
        version = Vsn
    }.


-spec lookup_dependency(RcptRev, DonorRep) -> Dep when
    RcptRev :: gitto_type:revision_id(),
    DonorRep :: gitto_type:revision_id(),
    Dep :: gitto_type:dependency().

lookup_dependency(RcptRev, DonorRep) ->
    Q = qlc:q([Dep || DonorRR = #g_repository_x_revision{} 
                              <- mnesia:table(g_repository_x_revision),
                      DonorRR.repository =:= DonorRep, 
                      Dep = #g_dependency{} 
                              <- mnesia:table(g_dependency),
                      Dep.donor     =:= DonorRR.revision,
                      Dep.recipient =:= RcptRev]),
    gitto_db:select1(Q).


-spec calculate_revision(DonorRevDesc, DonorRep, RcptRevDate) -> RevId when
    DonorRevDesc :: term(),
    DonorRep :: gitto_type:repository(),
    RcptRevDate :: gitto_type:timestamp(),
    RevId :: gitto_type:revision_id().

calculate_revision({tag, TagName}, #g_repository{id = RepId}, _) ->
    TagName1 = unicode:characters_to_binary(TagName),
    Q = qlc:q([Tag.revision || Tag = #g_tag{} <- mnesia:table(g_tag),
                               Tag.repository =:= RepId,
                               Tag.name =:= TagName1]),
    gitto_db:select1(Q);
calculate_revision(RevDesc, #g_repository{id = RepId}, Date) 
    when RevDesc =:= "HEAD"; RevDesc =:= {branch, "master"} ->
    lookup_first_parent_revision_by_date(RepId, Date);
calculate_revision({branch, _Branch}, #g_repository{}, _) ->
    %% TODO: calculate for other branches.
    undefined;
calculate_revision(RevisionHash, Rep = #g_repository{}, _) ->
    partical_revision_id(to_id(Rep), list_to_binary(RevisionHash)).


repository_x_revision(Rep, Rev, IsFirstParent) ->
    #g_repository_x_revision{repository = to_id(Rep), 
                             revision = to_id(Rev),
                             is_first_parent = IsFirstParent}.


revision_date_index(#g_repository{id = RepId}, 
                    #g_revision{id = RevId, committer_date = Date}) ->
    #g_revision_date_index{id = {RepId, Date}, revision = RevId}.


lookup_first_parent_revision_by_date(RepId, Date) ->
    F = fun() ->
            case gitto_db:read(g_revision_date_index, {RepId, Date}) of
            %% Matched by key (it is rare case).
            [#g_revision_date_index{revision = RevId}] -> RevId;

            [] -> 
                %% Get something before this date.
                case mnesia:prev(g_revision_date_index, {RepId, Date}) of
                    %% No entries for this table.
                    '$end_of_table' -> undefined;
                    %% Something was before this Key.
                    Key -> 
                        case hd(gitto_db:read(g_revision_date_index, Key)) of
                            #g_revision_date_index{id = {RepId, _Date}, 
                                                   revision = RevId} -> RevId;
                            %% It is a revision from another repository.
                            %% No valid revisions for this repository.
                            _ -> undefined
                        end
                end
            end
        end,
    gitto_db:transaction(F).
