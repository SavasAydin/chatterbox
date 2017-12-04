-module(username).
-export([subscribe/2
	]).

subscribe(User, Users) ->
    case lists:member(User, Users) of
	false ->
	    spawn(fun() -> init_user(User) end),
	    {[User | Users], subscribed}
    end.

init_user(User) ->
    register(User, self()),
    loop_user().

loop_user() ->
    receive
	_ ->
	    loop_user()
    end.
