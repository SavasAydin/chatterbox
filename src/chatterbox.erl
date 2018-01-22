-module(chatterbox).
-export([start/0, stop/0]).

start() ->
    [ ok = application:start(App) || App <- [crypto, chatterbox] ].


stop() ->
    application:stop(chatterbox).
