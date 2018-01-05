-module(room).

-export([start_room_process/2,
	 stop_room_process/1
	]).

start_room_process(Username, Roomname) ->
    spawn(fun() ->
		  chatterbox_lib:register_process(Roomname, self()),
		  room_loop([Username])
	  end).

stop_room_process(Roomname) ->
    chatterbox_lib:to_process_name(Roomname) ! stop.

room_loop(Users) ->
    receive
	{Pid, list_users} ->
	    Pid ! {users, Users},
	    room_loop(Users);

	{Pid, join, Username} ->
	    Message = "account is joined the room",
	    Pid ! {new_message_is_received, Message},
	    room_loop([Username | Users]);

	stop ->
	    ok
    end.
