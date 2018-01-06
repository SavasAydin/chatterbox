-module(chatterbox_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 12345).

start_chatterbox_server_test() ->
    ok = application:start(chatterbox),
    ?assert(is_pid(whereis(chatterbox_server))),
    ok = application:stop(chatterbox).

start_listening_on_chatterbox_server_test() ->
    ok = application:start(chatterbox),
    Res = chatterbox_server:get_listening_socket(),
    ?assertEqual({ok, ?PORT}, inet:port(Res)),
    ok = application:stop(chatterbox).

stop_chatterbox_server_test() ->
    ok = application:start(chatterbox),
    ok = application:stop(chatterbox),
    ?assertNot(is_pid(whereis(chatterbox_server))).

stop_listening_on_chatterbox_server_test() ->
    ok = application:start(chatterbox),
    LSock = chatterbox_server:get_listening_socket(),
    ok = application:stop(chatterbox),
    ?assertMatch({error, _}, inet:port(LSock)).
