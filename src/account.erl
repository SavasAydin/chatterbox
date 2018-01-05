-module(account).

-export([start_account_process/2,
	 stop_account_process/1,
	 list_room_users/1,
	 send/1,
	 join_room/1
	]).

start_account_process(Username, Socket) ->
    spawn(fun() ->
		  chatterbox_lib:register_process(Username, self()),
		  account_loop(Socket)
	  end).

stop_account_process(Username) ->
    chatterbox_lib:to_process_name(Username) ! stop.

list_room_users([Username, Roomname]) ->
    chatterbox_lib:to_process_name(Username) ! {list_users, Roomname},
    no_reply.

send([Username, Message]) ->
    PN = chatterbox_lib:to_process_name(Username),
    PN ! {new_message_is_received, Message},
    "sent".

join_room([Username, Roomname]) ->
    chatterbox_lib:to_process_name(Username) ! {join_room, Username, Roomname},
    no_reply.

account_loop(Socket) ->
    receive
	{new_message_is_received, Msg} ->
	    gen_tcp:send(Socket, term_to_binary(Msg)),
	    account_loop(Socket);

	{list_users, Room} ->
	    chatterbox_lib:to_process_name(Room) ! {self(), list_users},
	    account_loop(Socket);
	{users, Users} ->
	    gen_tcp:send(Socket, term_to_binary(Users)),
	    account_loop(Socket);

	{join_room, Username, Room} ->
	    chatterbox_lib:to_process_name(Room) ! {self(), join, Username},
	    account_loop(Socket);

	stop ->
	    ok
    end.
