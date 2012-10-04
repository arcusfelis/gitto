-module(gitto_type).
-export_type([person_id/0, 
              repository_id/0, 
              project_id/0, 
              address_id/0, 
              repository_id/0, 
              timestamp/0, 
              hash/0]).


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

