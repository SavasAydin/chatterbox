-module(account_server).

-behaviour(gen_server).

-export([start_link/0,
         create/1,
         is_created/1,
         delete/1,
         login/1,
         is_logged_in/1,
         logout/1,
         register_broadcaster/1,
         broadcast/2
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("chatterbox.hrl").

-define(SERVER, ?MODULE).

-record(state, {broadcaster, pids = []}).

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

broadcast(Username, Pid) ->
    gen_server:cast(?MODULE, {broadcast, Username, Pid}).

%%--------------------------------------------------------------------
handle_call({create, Args}, _, State) ->
    Tags = ["name", "password"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = create_if_not_exist(NewArgs),
    {reply, Reply, State};

handle_call({is_created, Args}, _, State) ->
    Tags = ["name"],
    [Username] = chatterbox_lib:get_values(Tags, Args),
    Reply = ets:lookup(accounts, Username) /= [],
    {reply, Reply, State};

handle_call({delete, Args}, _, State) ->
    Tags = ["name", "password"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = delete_if_exists(NewArgs),
    {reply, Reply, State};

handle_call({login, Args}, _, State) ->
    Tags = ["pid", "name", "password"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = handle_login(NewArgs),
    Pids = State#state.pids,
    {reply, Reply, State#state{pids = [hd(NewArgs) | Pids]}};

handle_call({is_logged_in, Args}, _, State) ->
    Tags = ["name"],
    [Username] = chatterbox_lib:get_values(Tags, Args),
    Reply = logged_in(Username),
    {reply, Reply, State};

handle_call({logout, Args}, _, State) ->
    Tags = ["pid", "name"],
    NewArgs = chatterbox_lib:get_values(Tags, Args),
    Reply = handle_logout(NewArgs),
    {reply, Reply, State};

handle_call(_, _, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast({register, Pid}, State) ->
    {noreply, State#state{broadcaster = Pid}};
handle_cast({broadcast, Username, Pid}, State) ->
    Pids = State#state.pids -- [Pid],
    Broadcaster = State#state.broadcaster,
    chatterbox_websocket:update(Broadcaster, Pids, Username),
    Logins = [atom_to_list(Name) || {Name, _} <- ets:tab2list(logins)],
    chatterbox_websocket:update(Broadcaster, [Pid], Logins -- [Username]),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_, _) ->
    ok.

%%--------------------------------------------------------------------
code_change(_, State, _) ->
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
            handle_logout([pid, Username]),
            true = ets:delete(accounts, Username),
            "account is deleted";
        [_] ->
            "username or password is wrong";
        [] ->
            "account was not created"
    end.

handle_login([Pid, Username, Password]) ->
    case logged_in(Username) of
        false ->
            login_if_authorized(Pid, Username, Password);
        true ->
            "already logged in"
    end.

login_if_authorized(Pid, Username, Password) ->
    case ets:lookup(accounts, Username) of
        [{Username, Password}] ->
            account:start_account_process(Username),
            chatterbox_debugger:increment_logged_accounts(),
            broadcast(Username, Pid),
            "logged in";
        [_] ->
            "username or password is wrong";
        [] ->
            "account must be created first"
    end.

handle_logout([_, Username]) ->
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
