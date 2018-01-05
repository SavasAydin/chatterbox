-module(account).

-export([start_account_process/2,
	 stop_account_process/1,
	 list_room_users/1,
	 send/1,
	 join_room/1
	]).

-include("chatterbox.hrl").

start_account_process(Username, Socket) ->
    spawn(fun() ->
		  register(?TO_ATOM(Username), self()),
		  account_loop(Socket)
	  end).

stop_account_process(Username) ->
    ?TO_ATOM(Username) ! stop.

list_room_users([Username, Roomname]) ->
    ?TO_ATOM(Username) ! {user_list_request, Roomname},
    no_reply.

send([Username, Message]) ->
    ?TO_ATOM(Username) ! {private_message, Message},
    "sent".

join_room([Username, Roomname]) ->
    ?TO_ATOM(Username) ! {join_room_request, Username, Roomname},
    no_reply.

account_loop(Socket) ->
    receive
	{private_message, Message} ->
	    gen_tcp:send(Socket, term_to_binary(Message)),
	    account_loop(Socket);

	{room_message, Message} ->
	    gen_tcp:send(Socket, term_to_binary(Message)),
	    account_loop(Socket);

	{user_list_request, Room} ->
	    ?TO_ATOM(Room) ! {self(), user_list_request},
	    account_loop(Socket);
	{user_list_response, Users} ->
	    gen_tcp:send(Socket, term_to_binary(Users)),
	    account_loop(Socket);

	{join_room_request, Username, Room} ->
	    ?TO_ATOM(Room) ! {self(), join_room_request, Username},
	    account_loop(Socket);
	{join_room_response, Response} ->
	    gen_tcp:send(Socket, term_to_binary(Response)),
	    account_loop(Socket);

	stop ->
	    ok
    end.
