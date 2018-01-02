-module(chatterbox_client).

-behaviour(gen_server).

-export([start_link/1,
	 stop/1,
	 start_chatterbox_server/0,
	 stop_chatterbox_server/0,
	 stop_chatterbox_server_if_running/0,
	 connect_to_chatterbox/0,
	 disconnect_from_chatterbox/0,
	 login/1,
	 logout/1,
	 is_logged_in/1
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {server,
		port,
		socket}).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []).

init(Port) ->
    process_flag(trap_exit, true),
    {ok, #state{port = Port}}.

stop(ServerRef) ->
    gen_server:stop(ServerRef).

start_chatterbox_server() ->
    gen_server:call(?MODULE, start_chatterbox_server).

stop_chatterbox_server() ->
    gen_server:call(?MODULE, stop_chatterbox_server).

stop_chatterbox_server_if_running() ->
    case whereis(chatterbox_server) of
	undefined ->
	    ok;
	_  ->
	    stop_chatterbox_server()
    end.

connect_to_chatterbox() ->
    gen_server:call(?MODULE, connect_to_chatterbox).

disconnect_from_chatterbox() ->
    gen_server:call(?MODULE, disconnect_from_chatterbox).

login(Username) ->
    gen_server:call(?MODULE, {login, Username}).

logout(Username) ->
    gen_server:call(?MODULE, {logout, Username}).

is_logged_in(Username) ->
    gen_server:call(?MODULE, {is_logged_in, Username}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(start_chatterbox_server, _, State) ->
    Port = State#state.port,
    {ok, Pid} = chatterbox_server:start_link(Port),
    {reply, success, State#state{server = Pid}};

handle_call(stop_chatterbox_server, _, State) ->
    Pid = State#state.server,
    ok = chatterbox_server:stop(Pid),
    {reply, success, State#state{server = undefined}};

handle_call(connect_to_chatterbox, _, State) ->
    IpAddress = get_ip_address(),
    Port = State#state.port,
    {ok, Socket} = gen_tcp:connect(IpAddress, Port, [binary]),
    {reply, success, State#state{socket = Socket}};

handle_call(disconnect_from_chatterbox, _, State) ->
    Socket = State#state.socket,
    ok = gen_tcp:close(Socket),
    {reply, success, State#state{socket = undefined}};

handle_call({login, Username}, _, State) ->
    Socket = State#state.socket,
    Command = "login " ++ Username,
    ct:pal("Command to send is ~p~n", [Command]),
    ok = gen_tcp:send(Socket, Command),
    Reply = receive_reply(Socket),
    {reply, Reply, State};

handle_call({logout, Username}, _, State) ->
    Socket = State#state.socket,
    Command = "logout " ++ Username,
    ct:pal("Command to send is ~p~n", [Command]),
    ok = gen_tcp:send(Socket, Command),
    Reply = receive_reply(Socket),
    {reply, Reply, State};

handle_call({is_logged_in, Username}, _, State) ->
    Socket = State#state.socket,
    Command = "is logged in " ++ Username,
    ct:pal("Command to send is ~p~n", [Command]),
    ok = gen_tcp:send(Socket, Command),
    Reply = receive_reply(Socket),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
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
terminate(_, State) ->
    Socket = State#state.socket,
    case is_port(Socket) of
	true ->
	    gen_tcp:close(Socket);
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
get_ip_address() ->
    {ok, Host} = inet:gethostname(),
    {ok, Address} = inet:getaddr(Host, inet),
    Address.

receive_reply(Socket) ->
    receive
	{tcp, Socket, Reply} ->
	    binary_to_term(Reply)
    after 100 ->
	    {error, client_timeout}
    end.
