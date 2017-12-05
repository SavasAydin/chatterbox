-module(chatterbox).
-export([start/0,
	 stop/0,
	 all_rooms/0,
	 all_users/0,
	 create/1,
	 destroy/1,
	 subscribe/1,
	 unsubscribe/1,
	 join/2,
	 list_users/1
	]).

-record(state, {rooms = [], users = []}).

start() ->
    spawn(fun init/0).

stop() ->
    chatterbox_lib:sync_call(chatterbox, stop).

all_rooms() ->
    chatterbox_lib:sync_call(chatterbox, all_rooms).

all_users() ->
    chatterbox_lib:sync_call(chatterbox, all_users).

create(Room) ->
    chatterbox_lib:sync_call(chatterbox, {create, Room}).

destroy(Room) ->
    chatterbox_lib:sync_call(chatterbox, {destroy, Room}).

subscribe(User) ->
    chatterbox_lib:sync_call(chatterbox, {subscribe, User}).

unsubscribe(User) ->
    chatterbox_lib:sync_call(chatterbox, {unsubscribe, User}).

join(Room, User) ->
    chatterbox_lib:sync_call(chatterbox, {join, Room, User}).

list_users(Room) ->
    chatterbox_lib:sync_call(chatterbox, {list_users, Room}).

init() ->
    register(chatterbox, self()),
    chatterbox(#state{}).

chatterbox(State) ->
    receive
	{{Ref, Pid}, stop} ->
	    Pid ! {Ref, ok};
	{From, Msg} ->
	    NewState = handle_call(Msg, From, State),
	    chatterbox(NewState)
    end.

handle_call(all_rooms, {Ref, Pid}, State) ->
    Pid ! {Ref, State#state.rooms},
    State;
handle_call(all_users, {Ref, Pid}, State) ->
    Pid ! {Ref, State#state.users},
    State;
handle_call({create, Room}, {Ref, Pid}, State) ->
    {NewRoom, Reply} = room:create(Room, State#state.rooms),
    Pid ! {Ref, Reply},
    State#state{rooms = NewRoom};
handle_call({destroy, Room}, {Ref, Pid}, State) ->
    {NewRooms, Reply} = room:destroy(Room, State#state.rooms),
    Pid ! {Ref, Reply},
    State#state{rooms = NewRooms};
handle_call({subscribe, User}, {Ref, Pid}, State) ->
    {NewUsers, Reply} = username:subscribe(User, State#state.users),
    Pid ! {Ref, Reply},
    State#state{users = NewUsers};
handle_call({unsubscribe, User}, {Ref, Pid}, State) ->
    {NewUsers, Reply} = username:unsubscribe(User, State#state.users),
    Pid ! {Ref, Reply},
    State#state{users = NewUsers};
handle_call({join, Room, User}, {Ref, Pid}, State) ->
    Reply = room:join(User, State#state.users, Room, State#state.rooms),
    Pid ! {Ref, Reply},
    State;
handle_call({list_users, Room}, {Ref, Pid}, State) ->
    Reply = room:list_users(Room, State#state.rooms),
    Pid ! {Ref, Reply},
    State.
