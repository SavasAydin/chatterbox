-module(room_server).

-behaviour(gen_server).

-export([start_link/0,
	 stop/1,
	 create/1,
	 is_created/1,
	 delete/1
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(room, {name, owner, users = []}).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    create_rooms_table_if_not_exist(),
    {ok, #state{}}.

stop(ServerRef) ->
    gen_server:stop(ServerRef).

create(Args) ->
    gen_server:call(?MODULE, {create, Args}).

is_created([_, Roomname]) ->
    gen_server:call(?MODULE, {is_created, Roomname}).

delete(Args) ->
    gen_server:call(?MODULE, {delete, Args}).

%%--------------------------------------------------------------------
handle_call({create, [Username, Roomname]}, _, State) ->
    Room = #room{name = Roomname, owner = Username, users = [Username]},
    true = ets:insert(rooms, Room),
    {reply, "room is created", State};

handle_call({is_created, Roomname}, _, State) ->
    Reply = ets:lookup(rooms, Roomname) /= [],
    {reply, Reply, State};

handle_call({delete, [_, Roomname]}, _, State) ->
    true = ets:delete(rooms, Roomname),
    {reply, "room is deleted", State};

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
create_rooms_table_if_not_exist() ->
    case ets:info(rooms) of
	undefined ->
	    create_rooms_table();
	_ ->
	    ok
    end.

create_rooms_table() ->
    ets:new(rooms, [public, named_table, {keypos, 2}]).
