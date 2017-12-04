-module(chatterbox).
-export([start/0,
	 stop/1,
	 init/2,
	 all/0,
	 create/1
	]).

start() ->
    start(make_ref(), self()).

stop(_) ->
    sync_call(stop).

all() ->
    sync_call(all).

create(Name) ->
    sync_call({create, Name}).

sync_call(Msg) ->
    Ref = make_ref(),
    chatterbox ! {Ref, self(), Msg},
    receive_reply(Ref).

receive_reply(Ref) ->
    receive
	{Ref, Reply} ->
	    Reply
    after 100 ->
	    {error, timeout}
    end.

start(Ref, Parent) ->
    Pid = spawn(?MODULE, init, [Ref, Parent]),
    started = receive_reply(Ref),
    {ok, Pid}.

init(Ref, Parent) ->
    register(chatterbox, self()),
    Parent ! {Ref, started},
    chatterbox([]).

chatterbox(Rooms) ->
    receive
	{Ref, Pid, stop} ->
	    Pid ! {Ref, ok};
	{Ref, Pid, Msg} ->
	    NewRooms = handle_call(Msg, {Ref, Pid}, Rooms),
	    chatterbox(NewRooms)
    end.

handle_call(all, {Ref, Pid}, Rooms) ->
    Pid ! {Ref, Rooms},
    Rooms;
handle_call({create, Name}, {Ref, Pid}, Rooms) ->
    {NewRooms, Reply} = handle_create_room(Name, Rooms),
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

create_room(Name) ->
    register(Name, self()),
    room_loop().

room_loop() ->
    receive
	_ ->
	    room_loop()
    end.
