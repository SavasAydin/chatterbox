-module(chatterbox_websocket_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8089).

start_and_stop_chatterbox_websocket_test() ->
    {ok, Pid} = chatterbox_websocket:start(?PORT),
    ?assert(is_process_alive(Pid)),
    ok = chatterbox_websocket:stop(),
    ?assertNot(is_process_alive(Pid)).

%% stop_chatterbox_websocket_test_() ->
%%     {setup,
%%      fun  start_if_not_running/0,
%%      fun(_) -> ok end,
%%      fun() ->
%%              ok = chatterbox_websocket:stop(),
%%              ?assertNot(is_running())
%%      end}.

%% is_running() ->
%%     Times = 50,
%%     check_if_chatterbox_websocket_is_running(Times).

%% check_if_chatterbox_websocket_is_running(0) ->
%%     false;
%% check_if_chatterbox_websocket_is_running(N) ->
%%     case whereis(chatterbox_websocket) of
%%         undefined ->
%%             timer:sleep(2),
%%             check_if_chatterbox_websocket_is_running(N-1);
%%         Pid ->
%%             is_pid(Pid)
%%     end.

%% start_if_not_running() ->
%%     case whereis(chatterbox_websocket) of
%%         undefined ->
%%             {ok, _} = chatterbox_websocket:start(?PORT);
%%         _ ->
%%             ok
%%     end.
