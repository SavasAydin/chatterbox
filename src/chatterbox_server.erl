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
    create_accounts_table_if_not_exist(),
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
    delete_accounts_table(),
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
create_accounts_table_if_not_exist() ->
    case ets:info(accounts) of
	undefined ->
	    create_accounts_table();
	_ ->
	    ok
    end.

create_accounts_table() ->
    ets:new(accounts, [public, named_table]).

delete_accounts_table() ->
    ets:delete(accounts).

accept_incoming_connections(LSock) ->
    Pid = spawn(fun() -> acceptor(LSock) end),
    erlang:monitor(process, Pid).

acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
	{ok, ASock} ->
	    gen_server:cast(?MODULE, accepted),
	    message_handler:loop(ASock);
	Error ->
	    Error
    end.
