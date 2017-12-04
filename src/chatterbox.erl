-module(chatterbox).
-export([start/0,
	 stop/1,
	 all/0,
	 create/1,
	 destroy/1,
	 join/2,
	 list_users/1
	]).

start() ->
    spawn(fun init/0).

stop(_) ->
    sync_call(chatterbox, stop).

all() ->
    sync_call(chatterbox, all).

create(Room) ->
    sync_call(chatterbox, {create, Room}).

destroy(Room) ->
    sync_call(chatterbox, {destroy, Room}).

join(Room, Nick) ->
    sync_call(chatterbox, {join, Room, Nick}).

list_users(Room) ->
    sync_call(chatterbox, {list_users, Room}).

sync_call(Where, Msg) ->
    Ref = make_ref(),
    Where ! {{Ref, self()}, Msg},
    receive_reply(Ref).

receive_reply(Ref) ->
    receive
	{Ref, Reply} ->
	    Reply
    after 100 ->
	    {error, timeout}
    end.

init() ->
    register(chatterbox, self()),
    chatterbox([]).

chatterbox(Rooms) ->
    receive
	{{Ref, Pid}, stop} ->
	    Pid ! {Ref, ok};
	{From, Msg} ->
	    NewRooms = handle_call(Msg, From, Rooms),
	    chatterbox(NewRooms)
    end.

handle_call(all, {Ref, Pid}, Rooms) ->
    Pid ! {Ref, Rooms},
    Rooms;
handle_call({create, Room}, {Ref, Pid}, Rooms) ->
    {NewRooms, Reply} = handle_create_room(Room, Rooms),
    Pid ! {Ref, Reply},
    NewRooms;
handle_call({destroy, Room}, {Ref, Pid}, Rooms) ->
    {NewRooms, Reply} = handle_destroy_room(Room, Rooms),
    Pid ! {Ref, Reply},
    NewRooms;
handle_call({join, Room, Nick}, {Ref, Pid}, Rooms) ->
    Reply = handle_join_room(Nick, Room, Rooms),
    Pid ! {Ref, Reply},
    Rooms;
handle_call({list_users, Room}, {Ref, Pid}, Rooms) ->
    Reply = handle_list_users(Room, Rooms),
    Pid ! {Ref, Reply},
    Rooms.    

handle_create_room(Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    {Rooms, {error, Room, already_created}};
	false ->
	    spawn(fun() -> create_room(Room) end),
	    {[Room | Rooms], created}
    end.

handle_destroy_room(Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    Reply = sync_call(Room, destroy),
	    {Rooms -- [Room], Reply};
	false ->
	    {Rooms, {error, Room, not_exist}}
    end.

handle_join_room(Nick, Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    sync_call(Room, {join, Nick});
	false ->
	    {error, Room, not_exist}
    end.

handle_list_users(Room, Rooms) ->
    case lists:member(Room, Rooms) of
	true ->
	    sync_call(Room, list_users);
	false ->
	    {error, Room, not_exist}
    end.
    
    
create_room(Name) ->
    register(Name, self()),
    room_loop([]).

room_loop(Users) ->
    receive
	{{Ref, Pid}, destroy} ->
	    Pid ! {Ref, destroyed};
	{{Ref, Pid}, {join, User}} ->
	    Pid ! {Ref, joined},	    
	    room_loop([User | Users]);
	{{Ref, Pid}, list_users} ->
	    Pid ! {Ref, Users},
	    room_loop(Users)
    end.
