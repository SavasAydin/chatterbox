-module(chatterbox_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 12345).

start_chatterbox_server_test() ->
    {ok, Pid} = chatterbox_server:start(?PORT),
    ?assert(is_started(Pid)),
    ok = chatterbox_server:stop(Pid),
    true = is_stopped().

start_listening_on_chatterbox_server_test() ->
    {ok, Pid} = chatterbox_server:start(?PORT),
    true = is_started(Pid),
    Res = chatterbox_server:get_listening_socket(),
    ?assertEqual({ok, ?PORT}, inet:port(Res)),
    ok = chatterbox_server:stop(Pid),
    true = is_stopped().

stop_chatterbox_server_test() ->
    {ok, Pid} = chatterbox_server:start(?PORT),
    true = is_started(Pid),
    ok = chatterbox_server:stop(Pid),
    ?assert(is_stopped()).

stop_listening_on_chatterbox_server_test() ->
    {ok, Pid} = chatterbox_server:start(?PORT),
    true = is_started(Pid),
    LSock = chatterbox_server:get_listening_socket(),
    ok = chatterbox_server:stop(Pid),
    true = is_stopped(),
    ?assertMatch({error, _}, inet:port(LSock)).

is_started(Pid) ->
    wait_max(Pid, 100).

is_stopped() ->
    wait_max(undefined, 100).

wait_max(Expected, 0) ->
    {error, Expected, not_received};
wait_max(Expected, N) ->
    case whereis(chatterbox_server) of
	Expected ->
	    true;
	_ ->
	    timer:sleep(1),
	    wait_max(Expected, N-1)
    end.
