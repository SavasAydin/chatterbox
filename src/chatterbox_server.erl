-module(chatterbox_server).

-behaviour(gen_server).

-export([start_link/1,
	 start/1,
	 get_listening_socket/0,
	 stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listen_socket}).

start(Port) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Port], []).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop(ServerRef) ->
    gen_server:stop(ServerRef).

get_listening_socket() ->
    gen_server:call(?MODULE, get_listening_socket).

init([Port]) ->
    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}]),
    accept_incoming_connections(LSock),
    {ok, #state{listen_socket = LSock}}.

handle_call(get_listening_socket, _, State) ->
    {reply, State#state.listen_socket, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, State) ->
    ok = gen_tcp:close(Socket),
    {noreply, State};
handle_info({tcp, Socket, <<"subscribe", Username/bitstring>>}, State) ->
    Reply = message_handler:subscribe(binary_to_atom(Username, latin1)),
    gen_tcp:send(Socket, Reply),
    {noreply, State};
handle_info({tcp, Socket, <<"list all users">>}, State) ->
    Reply = message_handler:all_users(),
    gen_tcp:send(Socket, Reply),
    {noreply, State};
handle_info({tcp, Socket, <<"unsubscribe", Username/bitstring>>}, State) ->
    Reply = message_handler:unsubscribe(binary_to_atom(Username, latin1)),
    gen_tcp:send(Socket, Reply),
    {noreply, State};
handle_info({'DOWN', Ref, process, _, _}, State) ->
    erlang:demonitor(Ref, [flush]),
    accept_incoming_connections(State#state.listen_socket),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    LSock = State#state.listen_socket,
    case is_port(LSock) of
	true ->
	    ok = gen_tcp:close(LSock);
	false ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept_incoming_connections(LSock) ->
    APid = spawn(fun() -> acceptor(LSock) end),
    erlang:monitor(process, APid).

acceptor(LSock) ->
    Chatterbox = whereis(?MODULE),
    case gen_tcp:accept(LSock) of
	{ok, ASock} ->
	    inet:setopts(ASock, [{active, true}]),
	    gen_tcp:controlling_process(ASock, Chatterbox),
	    acceptor(LSock);
	Error ->
	    Error
    end.
