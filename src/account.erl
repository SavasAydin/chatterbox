-module(account).

-export([create_table/0,
         start_account_process/1,
         stop_account_process/1,
         list_room_users/1,
         send/1,
         join_room/1
        ]).

-include("chatterbox.hrl").

create_table() ->
    chatterbox_lib:create_table_if_not_exist(accounts).

start_account_process(Username) ->
    spawn(fun() ->
                  register(?TO_ATOM(Username), self()),
                  account_loop()
          end).

stop_account_process(Username) ->
    ?TO_ATOM(Username) ! stop.

list_room_users([Username, Roomname]) ->
    ?TO_ATOM(Username) ! {user_list_request, Roomname},
    no_reply.

send([{"name", To}, {From, Message}]) ->
    ?TO_ATOM(To) ! {private_message, {From, Message}},
    sent.

join_room([Username, Roomname]) ->
    ?TO_ATOM(Username) ! {join_room_request, Username, Roomname},
    no_reply.

account_loop() ->
    receive
        {private_message, {From, Message}} ->
            send_to_client({From, Message}),
            account_loop();

        {room_message, Message} ->
            send_to_client(Message),
            account_loop();

        {user_list_request, Room} ->
            ?TO_ATOM(Room) ! {self(), user_list_request},
            account_loop();
        {user_list_response, Users} ->
            send_to_client({"users", Users}),
            account_loop();

        {join_room_request, Username, Room} ->
            ?TO_ATOM(Room) ! {self(), join_room_request, Username},
            account_loop();
        {join_room_response, Response} ->
            send_to_client({"room response", Response}),
            account_loop();

        stop ->
            ok
    end.

send_to_client(_)->
    send_to_client_using_http_connection.
