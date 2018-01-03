-module(user_server).

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

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    create_accounts_table_if_not_exist(),
    {ok, #state{}}.

stop(ServerRef) ->
    gen_server:stop(ServerRef).

create(Username) ->
    gen_server:call(?MODULE, {create, Username}).

is_created(Username) ->
    gen_server:call(?MODULE, {is_created, Username}).

delete(Username) ->
    gen_server:call(?MODULE, {delete, Username}).

%%--------------------------------------------------------------------
handle_call({create, Username}, _, State) ->
    true = ets:insert(accounts, {Username, password, false}),
    {reply, "account is created", State};

handle_call({is_created, Username}, _, State) ->
    Reply = ets:lookup(accounts, Username) /= [],
    {reply, Reply, State};

handle_call({delete, Username}, _, State) ->
    true = ets:delete(accounts, Username),
    {reply, "account is deleted", State};

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
