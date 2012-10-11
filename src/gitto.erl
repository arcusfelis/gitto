-module(gitto).
-export([start/0, stop/0]).

start() ->
    application:start(gitto).


stop() ->
    application:stop(gitto).
