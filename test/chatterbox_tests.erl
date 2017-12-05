-module(chatterbox_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_chatterbox_server_test() ->
    Pid = chatterbox:start(),
    ?assert(is_started(Pid)),
    ok = chatterbox:stop(),
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

chatterbox_room_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun create_a_chat_room_with_a_name/0,
      fun list_all_rooms/0,
      fun try_to_create_an_existing_chat_room/0,
      fun destroy_an_existing_chat_room_with_the_name/0,
      fun try_to_destroy_non_existing_chat_room/0
      ]}.

create_a_chat_room_with_a_name() ->
    Room = room_1,
    Res = chatterbox:create(Room),
    ?assertEqual(created, Res),
    ?assert(is_process_alive(whereis(Room))).

list_all_rooms() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Res = chatterbox:all_rooms(),
    ?assertEqual([room_1], Res).

try_to_create_an_existing_chat_room() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Res = chatterbox:create(Room),
    ?assertEqual({error, Room, already_exist}, Res),
    ?assertEqual([Room], chatterbox:all_rooms()).

destroy_an_existing_chat_room_with_the_name() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Res = chatterbox:destroy(Room),
    ?assertEqual(destroyed, Res),
    ?assertEqual([], chatterbox:all_rooms()),
    ?assertEqual(undefined, whereis(Room)).

try_to_destroy_non_existing_chat_room() ->
    Room = room_1,
    Res = chatterbox:destroy(Room),
    ?assertEqual({error, Room, not_exist}, Res).

chatterbox_user_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun subscribe_to_chatterbox_server_with_a_username/0,
      fun list_all_users/0,
      fun unsubscribe_from_chatterbox_with_a_username/0,
      fun try_to_subscribe_already_existing_username/0,
      fun try_to_unsubscribe_non_existing_username/0
     ]}.

subscribe_to_chatterbox_server_with_a_username() ->
    Username = username1,
    Res = chatterbox:subscribe(Username),
    ?assertEqual(subscribed, Res),
    ?assert(is_pid(whereis(Username))).

list_all_users() ->
    Username = username1,
    subscribed = chatterbox:subscribe(Username),
    Res = chatterbox:all_users(),
    ?assertEqual([Username], Res).

unsubscribe_from_chatterbox_with_a_username() ->
    Username = username1,
    subscribed = chatterbox:subscribe(Username),
    Res = chatterbox:unsubscribe(Username),
    ?assertEqual(unsubscribed, Res),
    ?assertNot(is_pid(whereis(Username))),
    ?assertEqual([], chatterbox:all_users()).

try_to_subscribe_already_existing_username() ->
    Username = username1,
    subscribed = chatterbox:subscribe(Username),
    Res = chatterbox:subscribe(Username),
    ?assertEqual({error, Username, already_exist}, Res).

try_to_unsubscribe_non_existing_username() ->
    Username = username1,
    Res = chatterbox:unsubscribe(Username),
    ?assertEqual({error, Username, not_exist}, Res).

chatterbox_mix_of_user_and_room_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun join_a_chat_room_with_a_username/0,
      fun list_users_in_a_chat_room/0,
      fun try_to_join_non_existing_chat_room/0,
      fun try_to_join_with_a_non_existing_username/0,
      fun try_to_list_users_in_non_existing_chat_room/0
     ]}.

join_a_chat_room_with_a_username() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Username = username_1,
    subscribed = chatterbox:subscribe(Username),
    Res = chatterbox:join(Room, Username),
    ?assertEqual(joined, Res),
    ?assertEqual([Username], chatterbox:list_users(Room)).

list_users_in_a_chat_room() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Username = username_1,
    subscribed = chatterbox:subscribe(Username),
    joined = chatterbox:join(Room, Username),
    ?assertEqual([Username], chatterbox:list_users(Room)).

try_to_join_non_existing_chat_room() ->
    Room = room_1,
    Username = username_1,
    subscribed = chatterbox:subscribe(Username),
    Res = chatterbox:join(Room, Username),
    ?assertEqual({error, Room, not_exist}, Res).

try_to_join_with_a_non_existing_username() ->
    Room = room_1,
    created = chatterbox:create(Room),
    Username = username_1,
    Res = chatterbox:join(Room, Username),
    ?assertEqual({error, Username, not_exist}, Res).

try_to_list_users_in_non_existing_chat_room() ->
    Room = room_1,
    Username = username_1,
    subscribed = chatterbox:subscribe(Username),
    {error, Room, not_exist} = chatterbox:join(Room, Username),
    ?assertEqual({error, Room, not_exist}, chatterbox:list_users(Room)).

setup() ->
    chatterbox:start().

cleanup(_) ->
    Rooms = chatterbox:all_rooms(),
    Users = chatterbox:all_users(),
    [chatterbox:destroy(Room) || Room <- Rooms],
    [chatterbox:unsubscribe(User) || User <- Users],
    chatterbox:stop().
