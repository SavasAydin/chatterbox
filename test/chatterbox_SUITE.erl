-module(chatterbox_SUITE).

-export([suite/0,
	 all/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2
	]).

-export([create_and_delete_account/1,
	 try_to_create_same_account_twice/1,
	 login_and_logout_once_created/1,
	 try_to_login_when_not_created/1,
	 try_to_login_with_wrong_password/1,
	 send_a_message/1
	]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8088).

%%--------------------------------------------------------------------
create_and_delete_account(_) ->
    Username = {"name", "Adam"},
    Password = {"password", "Password"},
    Actions = [{{create_account, [Username, Password]}, "account is created"},
	       {{is_account_created, Username}, true},
	       {{delete_account, Username}, "account is deleted"},
	       {{is_account_created, Username}, false}	       
	      ],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_create_same_account_twice(_) ->
    Username = {"name", "Adam"},
    Password = {"password", "Password"},
    Actions = [{{create_account, [Username, Password]}, "account is created"},
	       {{create_account, [Username, Password]}, "username is taken"},
	       {{delete_account, Username}, "account is deleted"}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
login_and_logout_once_created(_) ->
    Username = {"name", "Adam"},
    Password = {"password", "Password"},
    Actions = [{{create_account, [Username, Password]}, "account is created"},
	       {{login, [Username, Password]}, "logged in"},
	       {{is_logged_in, Username}, true},
	       {{logout, Username}, "logged out"},
	       {{is_logged_in, Username}, false},
	       {{delete_account, Username}, "account is deleted"}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_login_when_not_created(_) ->
    Username = {"name", "Adam"},
    Password = {"password", "Password"},
    Actions = [{{login, [Username, Password]}, "account must be created first"},
	       {{is_logged_in, Username}, false}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
try_to_login_with_wrong_password(_) ->
    Username = {"name", "Adam"},
    Password = {"password", "Password"},
    WrongPasssword = {"password", "Wrong"},
    Actions = [{{create_account, [Username, Password]}, "account is created"},
	       {{login, [Username, WrongPasssword]}, "username or password is wrong"},
	       {{is_logged_in, Username}, false}],
    perform_actions(Actions).

%%--------------------------------------------------------------------
send_a_message(_) ->
    Username1 = {"name", "Adam"},
    Password1 = {"password", "Password1"},
    Username2 = {"name", "Carol"},
    Password2 = {"password", "Password2"},
    Actions = [{{create_account, [Username1, Password1]}, "account is created"},
	       {{login, [Username1, Password1]}, "logged in"},
	       {{create_account, [Username2, Password2]}, "account is created"},
	       {{login, [Username2, Password2]}, "logged in"},
	       {{send_to_account, [Username2, {Username1, "hello"}]}, sent},
	       {{logout, Username1}, "logged out"},
	       {{logout, Username2}, "logged out"}],
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
    {ok, _} = chatterbox_client:start_link(),
    ok = application:set_env(chatterbox, port, ?PORT),
    ok = application:start(chatterbox),
    Config.

end_per_testcase(_TestCase, Config) ->
    ok = application:stop(chatterbox),
    Config.

all() ->
    [create_and_delete_account,
     try_to_create_same_account_twice,
     login_and_logout_once_created,
     try_to_login_when_not_created,
     try_to_login_with_wrong_password,
     send_a_message
    ].
