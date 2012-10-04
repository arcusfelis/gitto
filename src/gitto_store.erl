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
    


