-module(chatterbox_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_chatterbox_server_test() ->
    {ok, Pid} = chatterbox:start(),
    ?assert(is_process_alive(Pid)),
    ok = chatterbox:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

chatterbox_commands_test_() ->
    {foreach,
     fun chatterbox:start/0,
     fun chatterbox:stop/1,
     [fun all_command_returns_list_of_created_room/0,
      fun create_a_chat_room_with_a_name/0,
      fun try_to_create_an_existing_chat_room/0,
      fun destroy_an_existing_chat_room_with_the_name/0,
      fun try_to_destroy_non_existing_chat_room/0
     ]}.

all_command_returns_list_of_created_room() ->
    Res = chatterbox:all(),
    ?assertEqual([], Res).

create_a_chat_room_with_a_name() ->
    Name = room_1,
    created = chatterbox:create(Name),
    ?assertEqual([room_1], chatterbox:all()),
    ?assert(is_process_alive(whereis(Name))),
    destroyed = chatterbox:destroy(Name).

try_to_create_an_existing_chat_room() ->
    Name = room_1,
    created = chatterbox:create(Name),
    Res = chatterbox:create(Name),
    ?assertEqual({error, already_created}, Res),
    ?assertEqual([Name], chatterbox:all()),
    destroyed = chatterbox:destroy(Name).

destroy_an_existing_chat_room_with_the_name() ->
    Name = room_1,
    created = chatterbox:create(Name),
    Res = chatterbox:destroy(Name),
    ?assertEqual(destroyed, Res),
    ?assertEqual([], chatterbox:all()),
    ?assertEqual(undefined, whereis(Name)).

try_to_destroy_non_existing_chat_room() ->
    Name = room_1,
    Res = chatterbox:destroy(Name),
    ?assertEqual({error, not_existing}, Res).
