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
    last_successful_connection_date :: gitto_type:timestamp() | undefined,
    %% Addresses with priority = 0 will be skipped, addresses with the maximum 
    %% priority will be checked first.
    priority = 1 :: non_neg_integer()
}).

-record(g_person, {
        id      :: gitto_type:person_id(),
        name    :: unicode:unicode_binary(),
        email   :: binary()
}).

%% Analyzed `*.app.src' file.
-record(g_application, {
    id,
    %% Name of the directory, `application_name.app.src'.
    name        :: unicode:unicode_binary(),
    description :: unicode:unicode_binary(),
    version     :: term(),
    hash        :: integer()
}).

-record(g_revision, {
        id              :: gitto_type:revision_id(),
        application     :: gitto_type:application_id(),

        %% Unique.
        commit_hash     :: gitto_type:hash(),
        author          :: gitto_type:person_id(),
        committer       :: gitto_type:person_id(),
        author_date     :: gitto_type:timestamp(),
        committer_date  :: gitto_type:timestamp(),

        subject         :: unicode:unicode_binary(),
        body            :: unicode:unicode_binary(),

        parents   = []  :: [gitto_type:revision_id()],
        
        status          :: downloaded | compiled
}).


%% Denormalized version of #g_revision.
%% It is not used as a table.
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
        id              :: gitto_type:repository_x_revision_id(),
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


-record(g_module, {
        id      :: gitto_type:module_id(),
        name    :: atom()
}).

-record(g_function, {
        id      :: gitto_type:function_id(),
        name    :: atom(),
        arity   :: 1 .. 255,
        module  :: gitto_type:module_id()
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

-record(g_counter, {
        table           :: gitto_type:table_name(),
        last_id         :: gitto_type:dependence_id()
}).


%% ```
%% [{<<"type">>,<<"repo">>},
%% {<<"created">>,<<"2011-03-22T08:29:56-07:00">>},
%% {<<"followers">>,89},
%% {<<"owner">>,<<"erlang-unicode">>},
%% {<<"username">>,<<"erlang-unicode">>},
%% {<<"created_at">>,<<"2011-03-22T08:29:56-07:00">>},
%% {<<"pushed_at">>,<<"2012-10-21T13:19:37-07:00">>},
%% {<<"pushed">>,<<"2012-10-21T13:19:37-07:00">>},
%% {<<"forks">>,4},
%% {<<"description">>,
%%  <<"Unicode eXtention for Erlang (Strings, Collation)">>},
%% {<<"fork">>,false},
%% {<<"size">>,124},
%% {<<"name">>,<<"ux">>},
%% {<<"private">>,false},
%% {<<"watchers">>,89},
%% {<<"language">>,<<"Erlang">>}]
%% '''

-record(gh_repository, {
        id          :: gitto_type:gh_repository_id(),
        fullname    :: gitto_type:gh_repository_fullname(),
        name        :: unicode:unicode_binary(),
        description :: unicode:unicode_binary(),
        owner       :: unicode:unicode_binary(),
        created     :: gitto_type:timestamp(),
        pushed      :: gitto_type:timestamp(),
        forks_count :: non_neg_integer(),
        %% Pairs `{OwnerLogin, RepName}'.
        forks        = [] :: [gitto_type:gh_repository_fullname()],
        dependencies = [] :: [gitto_type:gh_repository_fullname()],
        %% Source fullnames of dependencies
        src_dependencies = [] :: [gitto_type:gh_repository_fullname()],
        watchers    :: non_neg_integer(),
        is_fork     :: boolean(),
        %% Size in KB.
        size        :: non_neg_integer(),

        template          :: apps | src | undefined,
        has_c_src_dir     :: boolean(),
        has_rel_dir       :: boolean(),
        has_priv_dir      :: boolean(),
        has_test_dir      :: boolean(),

        
        %% The name of the repository, from which this repository is forked.
        parent_fullname   :: gitto_type:gh_repository_fullname(),
        %% Root repository.
        source_fullname   :: gitto_type:gh_repository_fullname(),

        %% If there is no rebar.config, than rebar_config is undefined.
        rebar_config
}).

