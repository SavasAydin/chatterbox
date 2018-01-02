-module(message_handler).
-export([loop/1,
	 create/1,
	 delete/1,
	 is_created/1,
	 login/1,
	 logout/1,
	 is_logged_in/1,
	 send/1
	]).

-define(SERVER, chatterbox_server).

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp_closed, Socket} ->
	    ok = gen_tcp:close(Socket);

	{tcp, Socket, Data} ->
	    {Command, Args} = binary_to_term(Data),
	    Reply = ?MODULE:Command(Args),
	    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
	    loop(Socket);

	{new_message_is_received, Msg} ->
	    gen_tcp:send(Socket, term_to_binary(Msg)),
	    loop(Socket)

	end.

create(Username) ->
    true = ets:insert(accounts, {Username, password, false}),
    "created".

is_created(Username) ->
    ets:lookup(accounts, Username) /= [].

delete(Username) ->
    true = ets:delete(accounts, Username),
    "deleted".

login(Username) ->
    case ets:update_element(accounts, Username, {3, true}) of
	true ->
	    register_process(Username),
	    "logged in";
	false ->
	    "not created"
    end.

logout(Username) ->
    true = ets:update_element(accounts, Username, {3, false}),
    unregister_process(Username),
    "logged out".

register_process(Username) when is_list(Username) ->
    register_process(list_to_atom(Username));
register_process(Username) ->
    register(Username, self()).

unregister_process(Username) when is_list(Username) ->
    unregister_process(list_to_atom(Username));
unregister_process(Username) ->
    unregister(Username).

is_logged_in(Username) ->
    case  ets:lookup(accounts, Username) of
	[] ->
	    false;
	[User] ->
	    element(3, User)
    end.

send([Username, Message]) ->
    list_to_atom(Username) ! {new_message_is_received, Message},
    "sent".
