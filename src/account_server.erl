-module(account_server).

-behaviour(gen_server).

-export([start_link/0,
	 stop/1,
	 create/1,
	 is_created/1,
	 delete/1,
	 login/1,
	 is_logged_in/1,
	 logout/1,
	 send/1
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    create_accounts_table_if_not_exist(),
    {ok, #state{}}.

stop(ServerRef) ->
    gen_server:stop(ServerRef).

create(User) ->
    gen_server:call(?MODULE, {create, User}).

is_created(Username) ->
    gen_server:call(?MODULE, {is_created, Username}).

delete(Username) ->
    gen_server:call(?MODULE, {delete, Username}).

login(Args) ->
    gen_server:call(?MODULE, {login, Args}).

is_logged_in(Username) ->
    gen_server:call(?MODULE, {is_logged_in, Username}).

logout(Username) ->
    gen_server:call(?MODULE, {logout, Username}).

send(Args) ->
    gen_server:call(?MODULE, {send, Args}).

%%--------------------------------------------------------------------
handle_call({create, [Username, Password]}, _, State) ->
    Reply = create_if_not_exist(Username, Password),
    {reply, Reply, State};

handle_call({is_created, Username}, _, State) ->
    Reply = ets:lookup(accounts, Username) /= [],
    {reply, Reply, State};

handle_call({delete, Username}, _, State) ->
    true = ets:delete(accounts, Username),
    {reply, "account is deleted", State};

handle_call({login, Args}, _, State) ->
    Reply = login_if_authorized(Args),
    {reply, Reply, State};

handle_call({is_logged_in, Username}, _, State) ->
    Reply = check_that_if_user_is_logged_in(Username),
    {reply, Reply, State};

handle_call({logout, Username}, _, State) ->
    true = ets:update_element(accounts, Username, {3, false}),
    stop_account_process(Username),
    {reply, "logged out", State};

handle_call({send, [Username, Message]}, _, State) ->
    list_to_atom(Username) ! {new_message_is_received, Message},
    {reply, "sent", State};

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
create_accounts_table_if_not_exist() ->
    case ets:info(accounts) of
	undefined ->
	    create_accounts_table();
	_ ->
	    ok
    end.

create_accounts_table() ->
    ets:new(accounts, [public, named_table]).

create_if_not_exist(Username, Password) ->
    case ets:lookup(accounts, Username) of
	[] ->
	    true = ets:insert(accounts, {Username, Password, false}),
	    "account is created";
	_ ->
	    "username is taken"
    end.

login_if_authorized([Socket, Username, Password]) ->
    case ets:lookup(accounts, Username) of
	[{Username, Password, false}] ->
	    true = ets:update_element(accounts, Username, {3, true}),
	    start_account_process(Username, Socket),
	    "logged in";
	[_] ->
	    "username or password is wrong";
	[] ->
	    "account must be created first"
    end.

start_account_process(Username, Socket) ->
    spawn(fun() ->
		  register_process(Username),
		  gen_tcp:controlling_process(Socket, self()),
		  account_loop(Socket)
	  end).

register_process(Username) when is_list(Username) ->
    register_process(list_to_atom(Username));
register_process(Username) ->
    register(Username, self()).

stop_account_process(Username) when is_list(Username) ->
    stop_account_process(list_to_atom(Username));
stop_account_process(Username) ->
    Username ! stop.

check_that_if_user_is_logged_in(Username) ->
    case  ets:lookup(accounts, Username) of
	[] ->
	    false;
	[User] ->
	    element(3, User)
    end.

account_loop(Socket) ->
    receive
	{tcp, Socket, Data} ->
	    {M, F, A} = binary_to_term(Data),
	    Reply = M:F(A),
	    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
	    account_loop(Socket);

	{new_message_is_received, Msg} ->
	    gen_tcp:send(Socket, term_to_binary(Msg)),
	    account_loop(Socket);

	stop ->
	    ok
    end.
