%% -----------------------------------------------------------------------
%% Record Definitions
%% -----------------------------------------------------------------------

-record(g_project, {
        id :: gitto_type:project_id(),
        name :: unicode:unicode_binary()
}).

-record(g_repository, {
        id :: gitto_type:repository_id(),
        project :: gitto_type:project_id(),
        updated :: gitto_type:timestamp() 
}).

-record(g_address, {
    id :: gitto_type:address_id(),
    repository :: gitto_type:repository_id(),
    %% Unique.
    url,

    is_dead :: boolean() | undefined,
    last_try_date :: gitto_type:timestamp() | undefined,
    last_successful_connection_date :: gitto_type:timestamp() | undefined
}).

-record(g_person, {
        id      :: gitto_type:person_id(),
        name    :: unicode:unicode_binary(),
        email   :: binary()
}).

-record(g_revision, {
        id              :: gitto_type:revision_id(),
        %% Unique.
        commit_hash     :: gitto_type:hash(),
        author          :: gitto_type:person_id(),
        committer       :: gitto_type:person_id(),
        author_date     :: gitto_type:timestamp(),
        committer_date  :: gitto_type:timestamp(),

        subject         :: unicode:unicode_binary(),
        body            :: unicode:unicode_binary(),

        parents   = []  :: [gitto_type:revision_id()]
}).


%% Denormalized version of #g_revision.
-record(g_improper_revision, {
        id              :: gitto_type:revision_id(),
        commit_hash     :: gitto_type:hash(),
        subject         :: unicode:unicode_binary(),
        body            :: unicode:unicode_binary(),

        author          :: gitto_type:person_id(),
        committer       :: gitto_type:person_id(),
        author_date     :: gitto_type:timestamp(),
        committer_date  :: gitto_type:timestamp(),

        parents         :: [gitto_type:revision_id()],

        committer_name,
        committer_email,

        author_name,
        author_email,

        parent_hashes
}).


%% recipient wants donor.
-record(g_dependency, {
        id              :: gitto_type:dependence_id(),
        recipient       :: gitto_type:revision_id(),
        donor           :: gitto_type:revision_id(),
        raw_data        :: tuple(),
        name            :: atom(),
        version         :: binary()
}).

%% This table contains revisions of the repository.
%% The count of entries with same `revision' field is much lesser than the ones
%% with the same `repository'.
-record(g_repository_x_revision, {
        id              :: gitto_type:dependence_id(),
        repository      :: gitto_type:repository_id(),
        revision        :: gitto_type:revision_id(),
        is_first_parent :: boolean()
}).

-record(g_tag, {
        id              :: gitto_type:dependence_id(),
        repository      :: gitto_type:repository_id(),
        revision        :: gitto_type:revision_id(),
        name            :: unicode:unicode_binary()
}).

%% [erlang-questions] Efficient way to select a range of data between 2 values?
%% http://erlang.org/pipermail/erlang-questions/2009-August/046253.html
%%
%% How do I create and use (or simulate) multi-column indexes in Erlang Mnesia
%% http://stackoverflow.com/questions/4557190/how-do-i-create-and-use-or-simulate-multi-column-indexes-in-erlang-mnesia
-record(g_revision_date_index, {
    %% This table is denormalized: the `id' field is not atomic.
    %%
    %% `{_, _}' is always less than `{_, _, _}' in Erlang.
    %% `{repository_id(), timestamp()}' is for commits is a first parent (usually 
    %% it is a master branch) of the repository.
    %% `{repository_id(), branch_id(), timestamp()}' is the other form.
    id :: {gitto_type:repository_id(), gitto_type:timestamp()},
    revision :: gitto_type:revision_id()
}).
