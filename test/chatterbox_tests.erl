-module(chatterbox_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_chatterbox_server_test() ->
    {ok, Pid} = chatterbox:start(),
    ?assert(is_process_alive(Pid)),
    ok = chatterbox:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

chatterbox_commands_test_() ->
    {setup,
     fun chatterbox:start/0,
     fun chatterbox:stop/1,
     [fun all_command_returns_list_of_created_room/0,
      fun create_a_chat_room_with_a_name/0
     ]}.

all_command_returns_list_of_created_room() ->
    Res = chatterbox:all(),
    ?assertEqual([], Res).

create_a_chat_room_with_a_name() ->
    Name = room_1,
    created = chatterbox:create(Name),
    ?assertEqual([room_1], chatterbox:all()),
    ?assert(is_process_alive(whereis(Name))).
