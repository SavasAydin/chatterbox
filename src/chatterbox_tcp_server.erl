-module(chatterbox_tcp_server).

-behaviour(gen_server).

-export([start_link/1,
	 get_listening_socket/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listen_socket,
		users = []}).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

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
    ok = gen_tcp:close(LSock).

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
	    accept_loop(ASock);
	Error ->
	    Error
    end.

accept_loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp_closed, Socket} ->
	    ok = gen_tcp:close(Socket);

	{tcp, Socket, Data} ->
	    {M, F, Args} = binary_to_term(Data),
	    NewArgs = inject_socket_if_command_is_login(F, Args, Socket),
	    Reply = M:F(NewArgs),
	    ok = send_reply_if_required(Socket, Reply),
	    accept_loop(Socket)

	end.

inject_socket_if_command_is_login(login, Args, Socket) ->
    [Socket | Args];
inject_socket_if_command_is_login(_, Args, _) ->
    Args.

send_reply_if_required(_, no_reply) ->
    ok;
send_reply_if_required(Socket, Reply) ->
    gen_tcp:send(Socket, term_to_binary(Reply)).
