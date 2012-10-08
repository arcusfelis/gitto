%% -----------------------------------------------------------------------
%% Record Definitions
%% -----------------------------------------------------------------------

-record(g_project, {
        id :: gitto_type:project_id(),
        name :: unicode:unicode_binary()
}).

-record(g_repository, {
        id :: gitto_type:repository_id(),
        project :: gitto_type:project_id()
}).

-record(g_address, {
    id :: gitto_type:address_id(),
    repository :: gitto_type:repository_id(),
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
-record(g_dependence, {
        id              :: gitto_type:dependence_id(),
        recipient       :: gitto_type:revision_id(),
        donor           :: gitto_type:revision_id(),
        raw_data
}).


-record(g_first_parent, {
        id              :: gitto_type:dependence_id(),
        repository      :: gitto_type:repository_id(),
        revision        :: gitto_type:revision_id()
}).


-record(g_tag, {
        id              :: gitto_type:dependence_id(),
        repository      :: gitto_type:repository_id(),
        revision        :: gitto_type:revision_id(),
        name            :: unicode:unicode_binary()
}).
