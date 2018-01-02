-module(chatterbox_SUITE).

-export([suite/0,
	 all/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2
	]).

-export([login_to_chatterbox_and_logout_from_the_server/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 12345).

login_to_chatterbox_and_logout_from_the_server(_) ->
    Username = "Adam",
    Actions = [{start_chatterbox_server, success},
	       {connect_to_chatterbox, success},
	       {{login, Username}, "logged in"},
	       {{is_logged_in, Username}, true},
	       {{logout, Username}, "logged out"},
	       {{is_logged_in, Username}, false},
	       {disconnect_from_chatterbox, success},
	       {stop_chatterbox_server, success}],
    perform_actions(Actions).

perform_actions([]) ->
    ok;
perform_actions([{{Action, Args}, Expected} | Actions]) ->
    Result = chatterbox_client:Action(Args),
    ?assertEqual(Expected, Result),
    perform_actions(Actions);
perform_actions([{Action, Expected} | Actions]) ->
    Result = chatterbox_client:Action(),
    ?assertEqual(Expected, Result),
    perform_actions(Actions).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = chatterbox_client:start_link(?PORT),
    [{client_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = chatterbox_client:stop_chatterbox_server_if_running(),
    Pid = proplists:get_value(client_pid, Config),
    ok  = chatterbox_client:stop(Pid),
    proplists:delete(client_pid, Config).

all() ->
    [login_to_chatterbox_and_logout_from_the_server].
