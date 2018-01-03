-module(chatterbox_SUITE).

-export([suite/0,
	 all/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2
	]).

-export([create_and_delete_account/1,
	 login_and_logout_once_created/1,
	 try_to_login_when_not_created/1,
	 send_a_message/1,
	 create_and_delete_chat_room/1
	]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 12345).

create_and_delete_account(_) ->
    Username = "Adam",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, Username}, "account is created"},
	       {{is_account_created, Username}, true},
	       {{delete_account, Username}, "account is deleted"},
	       {{is_account_created, Username}, false},
	       {{disconnect_from_chatterbox, Username}, success}
	      ],
    perform_actions(Actions).

login_and_logout_once_created(_) ->
    Username = "Adam",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, Username}, "account is created"},
	       {{login, Username}, "logged in"},
	       {{is_logged_in, Username}, true},
	       {{logout, Username}, "logged out"},
	       {{is_logged_in, Username}, false},
	       {{delete_account, Username}, "account is deleted"},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

try_to_login_when_not_created(_) ->
    Username = "Adam",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{login, Username}, "account must be created first"},
	       {{is_logged_in, Username}, false},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

send_a_message(_) ->
    Username1 = "Adam",
    Username2 = "Carol",
    Actions = [{{connect_to_chatterbox, Username1}, success},
	       {{create_account, Username1}, "account is created"},
	       {{login, Username1}, "logged in"},
	       {{connect_to_chatterbox, Username2}, success},
	       {{create_account, Username2}, "account is created"},
	       {{login, Username2}, "logged in"},
	       {{send_message, {Username1, to, Username2, "hello"}}, ok},
	       {{receive_from_socket, Username1}, "sent"},
	       {{receive_from_socket, Username2}, "hello"},
	       {{disconnect_from_chatterbox, Username1}, success},
	       {{disconnect_from_chatterbox, Username2}, success}],
    perform_actions(Actions).

create_and_delete_chat_room(_) ->
    Username = "Adam",
    Room = "Chugginton",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, Username}, "account is created"},
	       {{login, Username}, "logged in"},
	       {{create_room, [Username, Room]}, "room is created"},
	       {{is_room_created, [Username, Room]}, true},
	       {{delete_room, [Username, Room]}, "room is deleted"},
	       {{is_room_created, [Username, Room]}, false},
	       {{logout, Username}, "logged out"},
	       {{delete_account, Username}, "account is deleted"},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
perform_actions([]) ->
    ok;
perform_actions([{{Action, Args}, Expected} | Actions]) ->
    ct:pal("Performing action ~p with args ~p~n", [Action, Args]),
    Result = chatterbox_client:Action(Args),
    ?assertEqual(Expected, Result),
    perform_actions(Actions);
perform_actions([{Action, Expected} | Actions]) ->
    ct:pal("Performing action ~p~n", [Action]),
    Result = chatterbox_client:Action(),
    ?assertEqual(Expected, Result),
    perform_actions(Actions).

%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = chatterbox_client:start_link(?PORT),
    ok = chatterbox_client:start_chatterbox_server(),
    ok = chatterbox_client:start_room_server(),
    ok = chatterbox_client:start_user_server(),
    [{client_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = chatterbox_client:stop_room_server(),
    ok = chatterbox_client:stop_user_server(),
    ok = chatterbox_client:stop_chatterbox_server(),
    Pid = proplists:get_value(client_pid, Config),
    ok  = chatterbox_client:stop(Pid),
    proplists:delete(client_pid, Config).

all() ->
    [create_and_delete_account,
     login_and_logout_once_created,
     try_to_login_when_not_created,
     send_a_message,
     create_and_delete_chat_room
    ].
