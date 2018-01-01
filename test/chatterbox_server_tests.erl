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

chatterbox_server_user_commands_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun subscribe_with_a_username/0,
      fun register_process_with_username_when_subscribing/0,
      fun list_all_users/0,
      fun unsubscribe_with_a_username/0,
      fun unregister_process_when_unsubscribing/0,
      fun send_message/0
     ]}.

subscribe_with_a_username() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Command = "subscribe Username",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("subscribed", Reply).

register_process_with_username_when_subscribing() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Command = "subscribe Username",
    ok = gen_tcp:send(Sock, Command),
    "subscribed" = receive_reply(Sock),
    ?assert(is_process_alive(whereis('Username'))).

list_all_users() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Subscribe = "subscribe Username",
    ok = gen_tcp:send(Sock, Subscribe),
    "subscribed" = receive_reply(Sock),
    Command = "list all users",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("Username", Reply).

unsubscribe_with_a_username() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Subscribe = "subscribe Username",
    ok = gen_tcp:send(Sock, Subscribe),
    "subscribed" = receive_reply(Sock),
    Command = "unsubscribe Username",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("unsubscribed", Reply).

unregister_process_when_unsubscribing() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Subscribe = "subscribe Username",
    ok = gen_tcp:send(Sock, Subscribe),
    "subscribed" = receive_reply(Sock),
    Command = "unsubscribe Username",
    ok = gen_tcp:send(Sock, Command),
    "unsubscribed" = receive_reply(Sock),
    ?assertEqual(undefined, whereis('Username')).

send_message() ->
    {ok, Sock1} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Subscribe1 = "subscribe Username1",
    ok = gen_tcp:send(Sock1, Subscribe1),
    "subscribed" = receive_reply(Sock1),

    {ok, Sock2} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Subscribe2 = "subscribe Username2",
    ok = gen_tcp:send(Sock2, Subscribe2),
    "subscribed" = receive_reply(Sock2),

    Command = "send Username2 hello from Username1",
    ok = gen_tcp:send(Sock1, Command),
    ReplyFromSock1 = receive_reply(Sock1),
    ?assertEqual("sent", ReplyFromSock1),

    ReplyFromSock2 = receive_reply(Sock2),
    ?assertEqual("hello from Username1", ReplyFromSock2).

receive_reply(Sock) ->
    receive
	{tcp, Sock, Reply} ->
	    Reply
    after 100 ->
	    {error, client_timeout}
    end.

setup() ->
    {ok, Pid} = chatterbox_server:start(?PORT),
    Pid.

cleanup(Pid) ->
    chatterbox_server:stop(Pid).
