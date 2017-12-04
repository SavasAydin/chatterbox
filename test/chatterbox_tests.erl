-module(chatterbox_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_chatterbox_server_test() ->
    Pid = chatterbox:start(),
    ?assert(erlang:is_process_alive(Pid)),
    stopped = chatterbox:stop(Pid),
    ?assertNot(erlang:is_process_alive(Pid)).
    
