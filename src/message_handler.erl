-module(message_handler).
-export([loop/1,
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
	    {M, F, A} = binary_to_term(Data),
	    Reply = M:F(A),
	    ok = gen_tcp:send(Socket, term_to_binary(Reply)),
	    loop(Socket);

	{new_message_is_received, Msg} ->
	    gen_tcp:send(Socket, term_to_binary(Msg)),
	    loop(Socket)

	end.

login([Username, Password]) ->
    case ets:lookup(accounts, Username) of
	[{Username, Password, false}] ->
	    true = ets:update_element(accounts, Username, {3, true}),
	    register_process(Username),
	    "logged in";
	[_] ->
	    "username or password is wrong";
	[] ->
	    "account must be created first"
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
