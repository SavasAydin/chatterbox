-module(account_server).

-behaviour(gen_server).

-export([start_link/0,
	 stop/1,
	 create/1,
	 is_created/1,
	 delete/1,
	 login/1,
	 is_logged_in/1,
	 logout/1
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
    Reply = chatterbox_lib:is_process_defined(Username),
    {reply, Reply, State};

handle_call({logout, Username}, _, State) ->
    account:stop_account_process(Username),
    {reply, "logged out", State};

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
    chatterbox_lib:create_table_if_not_exist(accounts).

create_if_not_exist(Username, Password) ->
    case ets:lookup(accounts, Username) of
	[] ->
	    true = ets:insert(accounts, {Username, Password}),
	    "account is created";
	_ ->
	    "username is taken"
    end.

login_if_authorized([Socket, Username, Password]) ->
    case ets:lookup(accounts, Username) of
	[{Username, Password}] ->
	    account:start_account_process(Username, Socket),
	    "logged in";
	[_] ->
	    "username or password is wrong";
	[] ->
	    "account must be created first"
    end.
