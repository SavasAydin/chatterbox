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
	 send_a_message/1
	]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 12345).

create_and_delete_account(_) ->
    Username = "Adam",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create, Username}, "created"},
	       {{is_created, Username}, true},
	       {{delete, Username}, "deleted"},
	       {{is_created, Username}, false},
	       {{disconnect_from_chatterbox, Username}, success}
	      ],
    perform_actions(Actions).

login_and_logout_once_created(_) ->
    Username = "Adam",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{create, Username}, "created"},
	       {{login, Username}, "logged in"},
	       {{is_logged_in, Username}, true},
	       {{logout, Username}, "logged out"},
	       {{is_logged_in, Username}, false},
	       {{delete, Username}, "deleted"},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

try_to_login_when_not_created(_) ->
    Username = "Adam",
    Actions = [{{connect_to_chatterbox, Username}, success},
	       {{login, Username}, "not created"},
	       {{is_logged_in, Username}, false},
	       {{disconnect_from_chatterbox, Username}, success}],
    perform_actions(Actions).

send_a_message(_) ->
    Username1 = "Adam",
    Username2 = "Carol",
    Actions = [{{connect_to_chatterbox, Username1}, success},
	       {{create, Username1}, "created"},
	       {{login, Username1}, "logged in"},
	       {{connect_to_chatterbox, Username2}, success},
	       {{create, Username2}, "created"},
	       {{login, Username2}, "logged in"},
	       {{send_message, {Username1, to, Username2, "hello"}}, ok},
	       {{receive_from_socket, Username1}, "sent"},
	       {{receive_from_socket, Username2}, "hello"},
	       {{disconnect_from_chatterbox, Username1}, success},
	       {{disconnect_from_chatterbox, Username2}, success}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
perform_actions([]) ->
    ok;
perform_actions([{{Action, Args}, Expected} | Actions]) ->
    ct:pal("Performing action ~p~n", [Action]),
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
    [{client_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = chatterbox_client:stop_chatterbox_server(),
    Pid = proplists:get_value(client_pid, Config),
    ok  = chatterbox_client:stop(Pid),
    proplists:delete(client_pid, Config).

all() ->
    [create_and_delete_account,
     login_and_logout_once_created,
     try_to_login_when_not_created,
     send_a_message
    ].
