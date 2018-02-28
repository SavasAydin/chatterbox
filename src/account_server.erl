-module(account_server).

-behaviour(gen_server).

-export([start_link/0,
         register_broadcaster/1,
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

-record(state, {broadcaster}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

register_broadcaster(Pid) ->
    gen_server:cast(?MODULE, {register, Pid}).

create(Args) ->
    gen_server:call(?MODULE, {create, Args}).

is_created(Username) ->
    gen_server:call(?MODULE, {is_created, Username}).

delete(Args) ->
    gen_server:call(?MODULE, {delete, Args}).

login(Args) ->
    gen_server:call(?MODULE, {login, Args}).

is_logged_in(Username) ->
    gen_server:call(?MODULE, {is_logged_in, Username}).

logout(Args) ->
    gen_server:call(?MODULE, {logout, Args}).

%%--------------------------------------------------------------------
handle_call({create, Args}, _, State) ->
    Tags = ["name", "password"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = create_if_not_exist(NewArgs),
    {reply, Reply, State};

handle_call({is_created, {"name", Username}}, _, State) ->
    Reply = ets:lookup(accounts, Username) /= [],
    {reply, Reply, State};

handle_call({delete, Args}, _, State) ->
    Tags = ["name", "password"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = delete_if_exists(NewArgs),
    {reply, Reply, State};

handle_call({login, Args}, _, State) ->
    Tags = ["name", "password"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Broadcaster = State#state.broadcaster,
    Reply = handle_login([Broadcaster | NewArgs]),
    {reply, Reply, State};

handle_call({is_logged_in, {"name", Username}}, _, State) ->
    Reply = logged_in(Username),
    {reply, Reply, State};

handle_call({logout, [{"name", Username}]}, _, State) ->
    Reply = handle_logout(Username),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast({register, Pid}, State) ->
    {noreply, State#state{broadcaster = Pid}};
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
create_if_not_exist([Username, Password]) ->
    case ets:lookup(accounts, Username) of
        [] ->
            true = ets:insert(accounts, {Username, Password}),
            chatterbox_debugger:increment_created_accounts(),
            "account is created";
        _ ->
            chatterbox_debugger:increment_failed_account_creation_attempts(),
            "username is taken"
    end.

delete_if_exists([Username, Password]) ->
    case ets:lookup(accounts, Username) of
        [{Username, Password}] ->
            handle_logout(Username),
            true = ets:delete(accounts, Username),
            "account is deleted";
        [_] ->
            "username or password is wrong";
        [] ->
            "account was not created"
    end.

handle_login([Broadcaster, Username, Password]) ->
    case logged_in(Username) of
        false ->
            login_if_authorized(Broadcaster, Username, Password);
        true ->
            "already logged in"
    end.

login_if_authorized(Broadcaster, Username, Password) ->
    case ets:lookup(accounts, Username) of
        [{Username, Password}] ->
            account:start_account_process(Username),
            chatterbox_debugger:increment_logged_accounts(),
            update_online_users(Broadcaster, Username),
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

logged_in(Username) ->
    whereis(?TO_ATOM(Username)) /= undefined.

update_online_users(Broadcaster, Username) ->
    spawn(fun() ->
                  timer:sleep(10),
                  chatterbox_websocket:update(Broadcaster, Username)
          end).
