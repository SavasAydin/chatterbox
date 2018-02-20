-module(room_server).

-behaviour(gen_server).

-export([start_link/0,
         create/1,
         is_created/1,
         delete/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(room, {name, owner}).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

create(Args) ->
    gen_server:call(?MODULE, {create, Args}).

is_created(Room) ->
    gen_server:call(?MODULE, {is_created, Room}).

delete(Args) ->
    gen_server:call(?MODULE, {delete, Args}).

%%--------------------------------------------------------------------
handle_call({create, Args}, _, State) ->
    Tags = ["name", "roomname"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = create_if_not_exist(NewArgs),
    {reply, Reply, State};

handle_call({is_created, {"roomname", Roomname}}, _, State) ->
    Reply = ets:lookup(rooms, Roomname) /= [],
    {reply, Reply, State};

handle_call({delete, Args}, _, State) ->
    Tags = ["name", "roomname"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = handle_deleting_room(NewArgs),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
create_if_not_exist([Name, Roomname]) ->
    case ets:lookup(rooms, Roomname) of
        [] ->
            Room = #room{name = Roomname, owner = Name},
            true = ets:insert(rooms, Room),
            room:start_room_process(Name, Roomname),
            chatterbox_debugger:increment_created_rooms(),
            "room is created";
        _ ->
            chatterbox_debugger:increment_failed_room_creation_attempts(),
            "roomname is taken"
    end.

handle_deleting_room([Name, Roomname]) ->
    case ets:lookup(rooms, Roomname) of
        [Room] ->
            delete_if_owner(Name, Room);
        [] ->
            "room must be created first"
    end.

delete_if_owner(Name, Room) ->
    case Room#room.owner of
        Name ->
            Roomname = Room#room.name,
            room:stop_room_process(Roomname),
            true = ets:delete(rooms, Roomname),
            "room is deleted";
        _ ->
            "only owner can delete"
    end.
