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
    chatterbox_lib:sync_call(chatterbox, stop).

all() ->
    chatterbox_lib:sync_call(chatterbox, all).

create(Room) ->
    chatterbox_lib:sync_call(chatterbox, {create, Room}).

destroy(Room) ->
    chatterbox_lib:sync_call(chatterbox, {destroy, Room}).

join(Room, Nick) ->
    chatterbox_lib:sync_call(chatterbox, {join, Room, Nick}).

list_users(Room) ->
    chatterbox_lib:sync_call(chatterbox, {list_users, Room}).

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
    {NewRooms, Reply} = room:create(Room, Rooms),
    Pid ! {Ref, Reply},
    NewRooms;
handle_call({destroy, Room}, {Ref, Pid}, Rooms) ->
    {NewRooms, Reply} = room:destroy(Room, Rooms),
    Pid ! {Ref, Reply},
    NewRooms;
handle_call({join, Room, Nick}, {Ref, Pid}, Rooms) ->
    Reply = room:join(Nick, Room, Rooms),
    Pid ! {Ref, Reply},
    Rooms;
handle_call({list_users, Room}, {Ref, Pid}, Rooms) ->
    Reply = room:list_users(Room, Rooms),
    Pid ! {Ref, Reply},
    Rooms.
