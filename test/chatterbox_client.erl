-module(chatterbox_client).

-behaviour(gen_server).

-export([start_link/1,
	 stop/1,
	 start_chatterbox_server/0,
	 stop_chatterbox_server/0,
	 start_room_server/0,
	 stop_room_server/0,
	 start_account_server/0,
	 stop_account_server/0,
	 connect_to_chatterbox/1,
	 disconnect_from_chatterbox/1,
	 create_account/1,
	 delete_account/1,
	 is_account_created/1,
	 login/1,
	 logout/1,
	 is_logged_in/1,
	 send_to_account/1,
	 send_to_room/1,
	 receive_from_socket/1,
	 create_room/1,
	 delete_room/1,
	 is_room_created/1,
	 list_room_users/1,
	 join_room/1
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {chatterbox_server,
		room_server,
		account_server,
		port,
		sockets = []}).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []).

init(Port) ->
    process_flag(trap_exit, true),
    {ok, #state{port = Port}}.

stop(ServerRef) ->
    gen_server:stop(ServerRef).

start_chatterbox_server() ->
    gen_server:call(?MODULE, {chatterbox_server, start}).

stop_chatterbox_server() ->
    stop_if_started(chatterbox_server).

start_room_server() ->
    gen_server:call(?MODULE, {room_server, start}).

stop_room_server() ->
    stop_if_started(room_server).

start_account_server() ->
    gen_server:call(?MODULE, {account_server, start}).

stop_account_server() ->
    stop_if_started(account_server).

connect_to_chatterbox(Username) ->
    gen_server:call(?MODULE, {connect_to_chatterbox, Username}).

disconnect_from_chatterbox(Username) ->
    gen_server:call(?MODULE, {disconnect_from_chatterbox, Username}).

create_account(Args) ->
    gen_server:call(?MODULE, {account_server, create, Args}).

delete_account(Username) ->
    gen_server:call(?MODULE, {account_server, delete, Username}).

is_account_created(Username) ->
    gen_server:call(?MODULE, {account_server, is_created, Username}).

login(Args) ->
    gen_server:call(?MODULE, {account_server, login, Args}).

logout(Username) ->
    gen_server:call(?MODULE, {account_server, logout, Username}).

is_logged_in(Username) ->
    gen_server:call(?MODULE, {account_server, is_logged_in, Username}).

send_to_account(Args) ->
    gen_server:call(?MODULE, {account, send, Args}).

send_to_room(Args) ->
    gen_server:call(?MODULE, {room, send, Args}).

receive_from_socket(Username) ->
    gen_server:call(?MODULE, {receive_from_socket, Username}).

create_room(Args) ->
    gen_server:call(?MODULE, {room_server, create, Args}).

delete_room(Args) ->
    gen_server:call(?MODULE, {room_server, delete, Args}).

is_room_created(Args) ->
    gen_server:call(?MODULE, {room_server, is_created, Args}).

list_room_users(Args) ->
    gen_server:call(?MODULE, {account, list_room_users, Args}).

join_room(Args) ->
    gen_server:call(?MODULE, {account, join_room, Args}).

%%--------------------------------------------------------------------
handle_call({chatterbox_server, start}, _, State) ->
    Port = State#state.port,
    {ok, Pid} = chatterbox_server:start_link(Port),
    {reply, ok, State#state{chatterbox_server = Pid}};

handle_call({chatterbox_server, stop}, _, State) ->
    Pid = State#state.chatterbox_server,
    ok = chatterbox_server:stop(Pid),
    {reply, ok, State#state{chatterbox_server = undefined}};

handle_call({room_server, start}, _, State) ->
    {ok, Pid} = room_server:start_link(),
    {reply, ok, State#state{room_server = Pid}};

handle_call({room_server, stop}, _, State) ->
    Pid = State#state.room_server,
    ok = room_server:stop(Pid),
    {reply, ok, State#state{room_server = undefined}};

handle_call({account_server, start}, _, State) ->
    {ok, Pid} = account_server:start_link(),
    {reply, ok, State#state{account_server = Pid}};

handle_call({account_server, stop}, _, State) ->
    Pid = State#state.account_server,
    ok = account_server:stop(Pid),
    {reply, ok, State#state{account_server = undefined}};

handle_call({connect_to_chatterbox, Username}, _, State) ->
    IpAddress = get_ip_address(),
    Port = State#state.port,
    {ok, Socket} = gen_tcp:connect(IpAddress, Port, [binary]),
    Sockets = State#state.sockets,
    {reply, success, State#state{sockets = [{Username, Socket} | Sockets]}};

handle_call({disconnect_from_chatterbox, Username}, _, State) ->
    Sockets = State#state.sockets,
    Socket = proplists:get_value(Username, Sockets),
    ok = gen_tcp:close(Socket),
    NewSockets = proplists:delete(Username, Sockets),
    {reply, success, State#state{sockets = NewSockets}};

handle_call({M, F, {Username1, to, Username2, Message}}, _, State) ->
    Sockets = State#state.sockets,
    Socket1 = proplists:get_value(Username1, Sockets),
    ok = gen_tcp:send(Socket1, term_to_binary({M, F, [Username2, Message]})),
    {reply, ok, State};

handle_call({receive_from_socket, Username}, _, State) ->
    Sockets = State#state.sockets,
    Socket = proplists:get_value(Username, Sockets),
    Reply = receive_reply(Socket),
    {reply, Reply, State};

handle_call(Command, _, State) ->
    ct:pal("Command to send is ~p~n", [Command]),
    Sockets = State#state.sockets,
    Username = get_username(Command),
    Socket = proplists:get_value(Username, Sockets),
    ok = gen_tcp:send(Socket, term_to_binary(Command)),
    Reply = receive_reply(Socket),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_, State) ->
    Sockets = State#state.sockets,
    [ close_sockets(Socket) || {_, Socket} <- Sockets ].

close_sockets(Socket) ->
    case is_port(Socket) of
	true ->
	    gen_tcp:close(Socket);
	false ->
	    ok
    end.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
get_ip_address() ->
    {ok, Host} = inet:gethostname(),
    {ok, Address} = inet:getaddr(Host, inet),
    Address.

get_username({_, _, [Username, _]}) ->
    Username;
get_username({_, _, Username}) ->
    Username.

receive_reply(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp, Socket, Reply} ->
	    binary_to_term(Reply)
    after 100 ->
	    {error, client_timeout}
    end.

stop_if_started(Server) ->
    case whereis(Server) of
	undefined ->
	    ok;
	_  ->
	    gen_server:call(?MODULE, {Server, stop})
    end.
