-module(chatterbox_server).

-behaviour(gen_server).

-export([start_link/1,
	 start/1,
	 get_listening_socket/0,
	 stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listen_socket,
		users = []}).

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
    Opts = [binary, {active, false}, {reuseaddr, true}],
    {ok, LSock} = gen_tcp:listen(Port, Opts),
    accept_incoming_connections(LSock),
    {ok, #state{listen_socket = LSock}}.

handle_call(get_listening_socket, _, State) ->
    {reply, State#state.listen_socket, State};

handle_call({login, Username}, _, State) ->
    Users = State#state.users,
    NewState = State#state{users = [Username | Users]},
    {reply, {ok, "logged in"}, NewState};

handle_call({logout, Username}, _, State) ->
    Users = State#state.users,
    NewState = State#state{users = Users -- [Username]},
    {reply, {ok, "logged out"}, NewState};

handle_call({is_logged_in, Username}, _, State) ->
    Users = State#state.users,
    Reply = lists:member(Username, Users),
    {reply, Reply, State};

handle_call(all_users, _, State) ->
    {reply, State#state.users, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(accepted, State) ->
    accept_incoming_connections(State#state.listen_socket),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, _}, State) ->
    erlang:demonitor(Ref, [flush]),
    accept_incoming_connections(State#state.listen_socket),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    LSock = State#state.listen_socket,
    case is_port(LSock) of
	true ->
	    ok = gen_tcp:close(LSock);
	false ->
	    ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
accept_incoming_connections(LSock) ->
    Pid = spawn(fun() -> acceptor(LSock) end),
    erlang:monitor(process, Pid).

acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
	{ok, ASock} ->
	    gen_server:cast(?MODULE, accepted),
	    loop(ASock);
	Error ->
	    Error
    end.

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp_closed, Socket} ->
	    ok = gen_tcp:close(Socket);
	{tcp, Socket, <<"login ", Username/bitstring>>} ->
	    Reply = login(binary_to_atom(Username, latin1)),
	    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
	    loop(Socket);
	{tcp, Socket, <<"list all users">>} ->
	    Reply = all_users(),
	    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
	    loop(Socket);
	{tcp, Socket, <<"logout ", Username/bitstring>>} ->
	    Reply = logout(binary_to_atom(Username, latin1)),
	    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
	    loop(Socket);
	{tcp, Socket, <<"is logged in ", Username/bitstring>>} ->
	    Reply = is_logged_in(binary_to_atom(Username, latin1)),
	    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
	    loop(Socket);
	{tcp, Socket, <<"send ", Data/bitstring>>} ->
	    [Username | Msg] = string:tokens(binary_to_list(Data), " "),
	    ok = gen_tcp:send(Socket, term_to_binary("sent")),
	    list_to_atom(Username) ! {message_is_received, Msg},
	    loop(Socket);
	{message_is_received, Msg} ->
	    gen_tcp:send(Socket, term_to_binary(string:join(Msg, " "))),
	    loop(Socket)
	end.

login(Username) ->
    {ok, Reply} = gen_server:call(?MODULE, {login, Username}),
    register(Username, self()),
    Reply.

logout(Username) ->
    {ok, Reply} = gen_server:call(?MODULE, {logout, Username}),
    unregister(Username),
    Reply.

is_logged_in(Username) ->
    gen_server:call(?MODULE, {is_logged_in, Username}).

all_users() ->
    gen_server:call(?MODULE, all_users).
