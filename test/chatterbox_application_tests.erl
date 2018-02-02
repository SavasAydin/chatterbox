-module(chatterbox_application_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8089).

start_chatterbox_server_test_() ->
    {setup,
     fun() ->
             ok = application:set_env(chatterbox, port, ?PORT)
     end,
     fun(_) ->
             ok = application:unset_env(chatterbox, port),
             ok = application:stop(chatterbox)
     end,
     fun() ->
             ok = application:start(chatterbox),
             Apps = application:which_applications(),
             ?assert(lists:keymember(chatterbox, 1, Apps))
     end}.

stop_chatterbox_server_test_() ->
    {setup,
     fun() ->
             ok = application:set_env(chatterbox, port, ?PORT),
             ok = application:start(chatterbox)
     end,
     fun(_) ->
             ok = application:unset_env(chatterbox, port)
     end,
     fun() ->
             ok = application:stop(chatterbox),
             Apps = application:which_applications(),
             ?assertNot(lists:keymember(chatterbox, 1, Apps))
     end}.
