-module(chatterbox_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_chatterbox_server_test() ->
    Pid = chatterbox:start(),
    ?assert(is_started(Pid)),
    ok = chatterbox:stop(Pid),
    ?assert(is_stopped()).

is_started(Pid) ->
    wait_max(Pid, 100).

is_stopped() ->
    wait_max(undefined, 100).

wait_max(Expected, 0) ->
    {error, Expected, not_received};
wait_max(Expected, N) ->
    case whereis(chatterbox) of
	Expected ->
	    true;
	_ ->
	    timer:sleep(1),
	    wait_max(Expected, N-1)
    end.

all_command_returns_list_of_created_room() ->
    Res = chatterbox:all(),
    ?assertEqual([], Res).

create_a_chat_room_with_a_name() ->
    Room = room_1,
    created = chatterbox:create(Room),
    ?assertEqual([room_1], chatterbox:all()),
    ?assert(is_process_alive(whereis(Room))),
    destroyed = chatterbox:destroy(Room).

try_to_create_an_existing_chat_room() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Res = chatterbox:create(Room),
    ?assertEqual({error, Room, already_created}, Res),
    ?assertEqual([Room], chatterbox:all()),
    destroyed = chatterbox:destroy(Room).

destroy_an_existing_chat_room_with_the_name() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Res = chatterbox:destroy(Room),
    ?assertEqual(destroyed, Res),
    ?assertEqual([], chatterbox:all()),
    ?assertEqual(undefined, whereis(Room)).

try_to_destroy_non_existing_chat_room() ->
    Room = room_1,
    Res = chatterbox:destroy(Room),
    ?assertEqual({error, Room, not_exist}, Res).

join_a_chat_room_with_a_nickname() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Nickname = nickname_1,
    Res = chatterbox:join(Room, Nickname),
    ?assertEqual(joined, Res),
    ?assertEqual([Nickname], chatterbox:list_users(Room)),
    destroyed = chatterbox:destroy(Room).

list_users_in_a_chat_room() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Nickname = nickname_1,
    joined = chatterbox:join(Room, Nickname),
    ?assertEqual([Nickname], chatterbox:list_users(Room)),
    destroyed = chatterbox:destroy(Room).

try_to_join_non_existing_chat_room() ->
    Room = room_1,
    Nickname = nickname_1,
    Res = chatterbox:join(Room, Nickname),
    ?assertEqual({error, Room, not_exist}, Res).

try_to_list_users_in_non_existing_chat_room() ->
    Room = room_1,
    Nickname = nickname_1,
    {error, Room, not_exist} = chatterbox:join(Room, Nickname),
    ?assertEqual({error, Room, not_exist}, chatterbox:list_users(Room)).

chatterbox_commands_test_() ->
    {setup,
     fun chatterbox:start/0,
     fun chatterbox:stop/1,
     [fun all_command_returns_list_of_created_room/0,
      fun create_a_chat_room_with_a_name/0,
      fun try_to_create_an_existing_chat_room/0,
      fun destroy_an_existing_chat_room_with_the_name/0,
      fun try_to_destroy_non_existing_chat_room/0,
      fun join_a_chat_room_with_a_nickname/0,
      fun list_users_in_a_chat_room/0,
      fun try_to_join_non_existing_chat_room/0,
      fun try_to_list_users_in_non_existing_chat_room/0
     ]}.
