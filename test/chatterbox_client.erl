-module(chatterbox_client).

-behaviour(gen_server).

-export([start_link/0,
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
		account_server}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_call({M, F, Args}, _, State) ->
    ct:pal("Command to send is ~p:~p(~p)~n", [M, F, Args]),
    Reply = M:F([{"pid", self()} | Args]),
    ct:pal("Reply is ~p~n", [Reply]),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_, _) ->
    ok.
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
