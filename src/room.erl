-module(room).
-export([create/2,
	 destroy/2,
	 join/3,
	 list_users/2
	]).

create(Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    {Rooms, {error, Room, already_created}};
	false ->
	    spawn(fun() -> init_room(Room) end),
	    {[Room | Rooms], created}
    end.

destroy(Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    Reply = chatterbox_lib:sync_call(Room, destroy),
	    {Rooms -- [Room], Reply};
	false ->
	    {Rooms, {error, Room, not_exist}}
    end.

join(Nick, Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    chatterbox_lib:sync_call(Room, {join, Nick});
	false ->
	    {error, Room, not_exist}
    end.

list_users(Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    chatterbox_lib:sync_call(Room, list_users);
	false ->
	    {error, Room, not_exist}
    end.

init_room(Name) ->
    register(Name, self()),
    loop([]).

loop(Users) ->
    receive
	{{Ref, Pid}, destroy} ->
	    Pid ! {Ref, destroyed};
	{{Ref, Pid}, {join, User}} ->
	    Pid ! {Ref, joined},
	    loop([User | Users]);
	{{Ref, Pid}, list_users} ->
	    Pid ! {Ref, Users},
	    loop(Users)
    end.
