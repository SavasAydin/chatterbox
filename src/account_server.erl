-module(account_server).

-behaviour(gen_server).

-export([start_link/0,
         create/1,
         is_created/1,
         delete/1,
         login/1,
         is_logged_in/1,
         logout/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("chatterbox.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_call({create, [{"name", Username}, {"password", Password}]}, _, State) ->
    Reply = create_if_not_exist(Username, Password),
    {reply, Reply, State};

handle_call({is_created, {"name", Username}}, _, State) ->
    Reply = ets:lookup(accounts, Username) /= [],
    {reply, Reply, State};

handle_call({delete, {"name", Username}}, _, State) ->
    true = ets:delete(accounts, Username),
    {reply, "account is deleted", State};

handle_call({login, [{"name", Username}, {"password", Password}]}, _, State) ->
    Reply = handle_login(Username, Password),
    {reply, Reply, State};

handle_call({is_logged_in, {"name", Username}}, _, State) ->
    Reply = logged_in(Username),
    {reply, Reply, State};

handle_call({logout, {"name", Username}}, _, State) ->
    Reply = handle_logout(Username),
    {reply, Reply, State};

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
create_if_not_exist(Username, Password) ->
    case ets:lookup(accounts, Username) of
        [] ->
            true = ets:insert(accounts, {Username, Password}),
            chatterbox_debugger:increment_created_accounts(),
            "account is created";
        _ ->
            chatterbox_debugger:increment_failed_account_creation_attempts(),
            "username is taken"
    end.

handle_login(Username, Password) ->
    case logged_in(Username) of
        false ->
            login_if_authorized(Username, Password);
        true ->
            "already logged in"
    end.

logged_in(Username) ->
    whereis(?TO_ATOM(Username)) /= undefined.

login_if_authorized(Username, Password) ->
    case ets:lookup(accounts, Username) of
        [{Username, Password}] ->
            account:start_account_process(Username),
            chatterbox_debugger:increment_logged_accounts(),
            "logged in";
        [_] ->
            "username or password is wrong";
        [] ->
            "account must be created first"
    end.

handle_logout(Username) ->
    case logged_in(Username) of
        true ->
            account:stop_account_process(Username),
            chatterbox_debugger:decrement_logged_accounts(),
            "logged out";
        false ->
            "not logged in"
    end.
