-module(chatterbox).
-export([start/0,
	 stop/1,
	 all/0,
	 create/1,
	 destroy/1
	]).

start() ->
    spawn(fun init/0).

stop(_) ->
    sync_call(chatterbox, stop).

all() ->
    sync_call(chatterbox, all).

create(Name) ->
    sync_call(chatterbox, {create, Name}).

destroy(Name) ->
    sync_call(chatterbox, {destroy, Name}).

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
handle_call({create, Name}, {Ref, Pid}, Rooms) ->
    {NewRooms, Reply} = handle_create_room(Name, Rooms),
    Pid ! {Ref, Reply},
    NewRooms;
handle_call({destroy, Name}, {Ref, Pid}, Rooms) ->
    {NewRooms, Reply} = handle_destroy_room(Name, Rooms),
    Pid ! {Ref, Reply},
    NewRooms.

handle_create_room(Name, Rooms) ->
    case lists:member(Name, Rooms) of
	true ->
	    {Rooms, {error, already_created}};
	false ->
	    spawn(fun() -> create_room(Name) end),
	    {[Name | Rooms], created}
    end.

handle_destroy_room(Name, Rooms) ->
    case lists:member(Name, Rooms) of
	true ->
	    Reply = sync_call(Name, destroy),
	    {Rooms -- [Name], Reply};
	false ->
	    {Rooms, {error, not_existing}}
    end.

create_room(Name) ->
    register(Name, self()),
    room_loop().

room_loop() ->
    receive
	{{Ref, Pid}, destroy} ->
	    Pid ! {Ref, destroyed}
    end.
