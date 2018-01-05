-module(chatterbox_SUITE).

-export([suite/0,
	 all/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2
	]).

-export([create_and_delete_account/1,
	 try_to_create_the_same_account_twice/1,
	 login_and_logout_once_created/1,
	 try_to_login_when_not_created/1,
	 try_to_login_with_wrong_password/1,
	 send_a_message/1,
	 create_and_delete_chat_room/1,
	 list_users_of_newly_created_room/1,
	 try_to_create_the_same_room_twice/1,
	 try_to_delete_room_when_not_owner/1,
	 join_a_chat_room/1
	]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 12345).

%%--------------------------------------------------------------------
create_and_delete_account(_) ->
    Username = "Adam",
    Password = "Password",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, [Username, Password]}, "account is created"},
	       {{is_account_created, Username}, true},
	       {{delete_account, Username}, "account is deleted"},
	       {{is_account_created, Username}, false},
	       {{disconnect_from_chatterbox, Username}, success}
	      ],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_create_the_same_account_twice(_) ->
    Username = "Adam",
    Password = "Password",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, [Username, Password]}, "account is created"},
	       {{create_account, [Username, Password]}, "username is taken"},
	       {{delete_account, Username}, "account is deleted"},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
login_and_logout_once_created(_) ->
    Username = "Adam",
    Password = "Password",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, [Username, Password]}, "account is created"},
	       {{login, [Username, Password]}, "logged in"},
	       {{is_logged_in, Username}, true},
	       {{logout, Username}, "logged out"},
	       {{is_logged_in, Username}, false},
	       {{delete_account, Username}, "account is deleted"},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_login_when_not_created(_) ->
    Username = "Adam",
    Password = "Password",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{login, [Username, Password]}, "account must be created first"},
	       {{is_logged_in, Username}, false},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_login_with_wrong_password(_) ->
    Username = "Adam",
    Password = "Password",
    WrongPasssword = "Wrong",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, [Username, Password]}, "account is created"},
	       {{login, [Username, WrongPasssword]}, "username or password is wrong"},
	       {{is_logged_in, Username}, false},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
send_a_message(_) ->
    Username1 = "Adam",
    Password1 = "Password1",
    Username2 = "Carol",
    Password2 = "Password2",
    Actions = [{{connect_to_chatterbox, Username1}, success},
	       {{create_account, [Username1, Password1]}, "account is created"},
	       {{login, [Username1, Password1]}, "logged in"},
	       {{connect_to_chatterbox, Username2}, success},
	       {{create_account, [Username2, Password2]}, "account is created"},
	       {{login, [Username2, Password2]}, "logged in"},
	       {{send_message, {Username1, to, Username2, "hello"}}, ok},
	       {{receive_from_socket, Username1}, "sent"},
	       {{receive_from_socket, Username2}, "hello"},
	       {{logout, Username1}, "logged out"},
	       {{logout, Username2}, "logged out"},
	       {{disconnect_from_chatterbox, Username1}, success},
	       {{disconnect_from_chatterbox, Username2}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
create_and_delete_chat_room(_) ->
    Username = "Adam",
    Room = "Chugginton",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_room, [Username, Room]}, "room is created"},
	       {{is_room_created, [Username, Room]}, true},
	       {{delete_room, [Username, Room]}, "room is deleted"},
	       {{is_room_created, [Username, Room]}, false},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
list_users_of_newly_created_room(_) ->
    Username = "Adam",
    Password = "Password",
    Room = "Chugginton",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_account, [Username, Password]}, "account is created"},
	       {{login, [Username, Password]}, "logged in"},
	       {{create_room, [Username, Room]}, "room is created"},
	       {{list_room_users, [Username, Room]}, [Username]},
	       {{delete_room, [Username, Room]}, "room is deleted"},
	       {{logout, Username}, "logged out"},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_create_the_same_room_twice(_) ->
    Username = "Adam",
    Room = "Wilson",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create_room, [Username, Room]}, "room is created"},
	       {{create_room, [Username, Room]}, "roomname is taken"},
	       {{delete_room, [Username, Room]}, "room is deleted"},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_delete_room_when_not_owner(_) ->
    Username1 = "Adam",
    Username2 = "Carol",
    Room = "Brewster",
    Actions = [{{connect_to_chatterbox, Username1}, success},
	       {{create_room, [Username1, Room]}, "room is created"},
	       {{connect_to_chatterbox, Username2}, success},
	       {{delete_room, [Username2, Room]}, "only owner can delete"},
	       {{is_room_created, [Username2, Room]}, true},
	       {{disconnect_from_chatterbox, Username1}, success},
	       {{disconnect_from_chatterbox, Username2}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
join_a_chat_room(_) ->
    Username1 = "Adam",
    Password1 = "Password1",
    Username2 = "Carol",
    Password2 = "Password2",
    Room = "Koko",
    Actions = [{{connect_to_chatterbox, Username1}, success},
	       {{create_account, [Username1, Password1]}, "account is created"},
	       {{login, [Username1, Password1]}, "logged in"},
	       {{create_room, [Username1, Room]}, "room is created"},
	       {{connect_to_chatterbox, Username2}, success},
	       {{create_account, [Username2, Password2]}, "account is created"},
	       {{login, [Username2, Password2]}, "logged in"},
	       {{join_room, [Username2, Room]}, "account is joined the room"},
	       {{list_room_users, [Username1, Room]}, [Username2, Username1]},
	       {{delete_room, [Username1, Room]}, "room is deleted"},
	       {{logout, Username1}, "logged out"},
	       {{logout, Username2}, "logged out"},
	       {{disconnect_from_chatterbox, Username1}, success},
	       {{disconnect_from_chatterbox, Username2}, success}],
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
    ok = chatterbox_client:start_account_server(),
    [{client_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = chatterbox_client:stop_room_server(),
    ok = chatterbox_client:stop_account_server(),
    ok = chatterbox_client:stop_chatterbox_server(),
    Pid = proplists:get_value(client_pid, Config),
    ok  = chatterbox_client:stop(Pid),
    proplists:delete(client_pid, Config).

all() ->
    [create_and_delete_account,
     try_to_create_the_same_account_twice,
     login_and_logout_once_created,
     try_to_login_when_not_created,
     try_to_login_with_wrong_password,
     send_a_message,
     create_and_delete_chat_room,
     list_users_of_newly_created_room,
     try_to_create_the_same_room_twice,
     try_to_delete_room_when_not_owner,
     join_a_chat_room
    ].
