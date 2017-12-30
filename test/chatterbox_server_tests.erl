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
     [fun subscribe/0,
      fun all_users/0,
      fun unsubscribe/0
     ]}.

subscribe() ->
    meck:expect(message_handler, subscribe, fun(_) -> "subscribed" end),
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Command = "subscribe Username",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("subscribed", Reply).

all_users() ->
    meck:expect(message_handler, all_users, fun() -> "username" end),
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Command = "list all users",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("username", Reply).

unsubscribe() ->
    meck:expect(message_handler, unsubscribe, fun(_) -> "unsubscribed" end),
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, []),
    Command = "unsubscribe Username",
    ok = gen_tcp:send(Sock, Command),
    Reply = receive_reply(Sock),
    ?assertEqual("unsubscribed", Reply).

receive_reply(Sock) ->
    receive
	{tcp, Sock, Reply} ->
	    Reply
    after 100 ->
	    {error, timeout}
    end.

setup() ->
    {ok, Pid} = chatterbox_server:start(?PORT),
    meck:new(message_handler, [non_strict]),
    Pid.

cleanup(Pid) ->
    chatterbox_server:stop(Pid),
    meck:unload(message_handler).
