-module(chatterbox_web_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CONFIG, [{ip, {0,0,0,0}},
                 {port, 8080},
                 {docroot, "priv/www/"}]).

start_chatterbox_web_server_test_() ->
    {setup,
     fun() -> ok end,
     fun stop_if_running/1,
     fun() ->
             {ok, Pid} = chatterbox_web_server:start(?CONFIG),
             ?assert(is_pid(Pid))
     end}.

stop_chatterbox_web_server_test_() ->
    {setup,
     fun  start_if_not_running/0,
     fun(_) -> ok end,
     fun() ->
             ok = chatterbox_web_server:stop(),
             ?assertNot(is_running())
     end}.

is_running() ->
    Times = 50,
    check_if_chatterbox_web_server_is_running(Times).

check_if_chatterbox_web_server_is_running(0) ->
    false;
check_if_chatterbox_web_server_is_running(N) ->
    case whereis(chatterbox_web_server) of
        undefined ->
            timer:sleep(2),
            check_if_chatterbox_web_server_is_running(N-1);
        Pid ->
            is_pid(Pid)
    end.

start_if_not_running() ->
    case whereis(chatterbox_web_server) of
        undefined ->
            {ok, _} = chatterbox_web_server:start(?CONFIG);
        _ ->
            ok
    end.

stop_if_running(_) ->
    case whereis(chatterbox_web_server) of
        undefined ->
            ok;
        _ ->
            ok = chatterbox_web_server:stop()
    end.
    
