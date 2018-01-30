-module(room).

-export([create_table/0,
         start_room_process/2,
         stop_room_process/1,
         send/1
        ]).

-include("chatterbox.hrl").

create_table() ->
    Opts = [public, named_table, {keypos, 2}],
    chatterbox_lib:create_table_if_not_exist(rooms, Opts).

start_room_process(Username, Roomname) ->
    spawn(fun() ->
                  register(?TO_ATOM(Roomname), self()),
                  room_loop([Username])
          end).

stop_room_process(Roomname) ->
    ?TO_ATOM(Roomname) ! stop.

send([Roomname, Message]) ->
    ?TO_ATOM(Roomname) ! {room_message, Message},
    "sent".

room_loop(Users) ->
    receive
        {Pid, user_list_request} ->
            Pid ! {user_list_response, Users},
            room_loop(Users);

        {Pid, join_room_request, Username} ->
            Response = Username ++ " is joined the room",
            Pid ! {join_room_response, Response},
            room_loop([Username | Users]);

        {room_message, Message} ->
            Msg = {room_message, Message},
            [ ?TO_ATOM(User) ! Msg || User <- Users ],
            room_loop(Users);

        stop ->
            ok
    end.
