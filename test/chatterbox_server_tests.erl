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
      fun login_to_the_server/0,
      fun user_process_is_registered_once_logged_in/0,
      fun list_all_users/0,
      fun logout_from_the_server/0,
      fun user_process_is_unregistered_once_logged_out/0,
      fun send_message/0
     ]}.

login_to_the_server() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary]),
    Command = "login Username",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("logged in", Reply).

user_process_is_registered_once_logged_in() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary]),
    Command = "login Username",
    ok = gen_tcp:send(Sock, Command),
    "logged in" = receive_reply(Sock),
    ?assert(is_process_alive(whereis('Username'))).

list_all_users() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary]),
    Login = "login Username",
    ok = gen_tcp:send(Sock, Login),
    "logged in" = receive_reply(Sock),
    Command = "list all users",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual(['Username'], Reply).

logout_from_the_server() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary]),
    Login = "login Username",
    ok = gen_tcp:send(Sock, Login),
    "logged in" = receive_reply(Sock),
    Command = "logout Username",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("logged out", Reply).

user_process_is_unregistered_once_logged_out() ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary]),
    Login = "login Username",
    ok = gen_tcp:send(Sock, Login),
    "logged in" = receive_reply(Sock),
    Command = "logout Username",
    ok = gen_tcp:send(Sock, Command),
    "logged out" = receive_reply(Sock),
    ?assertEqual(undefined, whereis('Username')).

send_message() ->
    {ok, Sock1} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary]),
    Login1 = "login Username1",
    ok = gen_tcp:send(Sock1, Login1),
    "logged in" = receive_reply(Sock1),

    {ok, Sock2} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary]),
    Login2 = "login Username2",
    ok = gen_tcp:send(Sock2, Login2),
    "logged in" = receive_reply(Sock2),

    Command = "send Username2 hello from Username1",
    ok = gen_tcp:send(Sock1, Command),
    ReplyFromSock1 = receive_reply(Sock1),
    ?assertEqual("sent", ReplyFromSock1),

    ReplyFromSock2 = receive_reply(Sock2),
    ?assertEqual("hello from Username1", ReplyFromSock2).

receive_reply(Sock) ->
    receive
	{tcp, Sock, Reply} ->
	    binary_to_term(Reply)
    after 100 ->
	    {error, client_timeout}
    end.

setup() ->
    {ok, Pid} = chatterbox_server:start(?PORT),
    Pid.

cleanup(Pid) ->
    chatterbox_server:stop(Pid).
