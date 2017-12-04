-module(username).
-export([subscribe/2,
	 unsubscribe/2
	]).

subscribe(User, Users) ->
    case lists:member(User, Users) of
	false ->
	    spawn(fun() -> init_user(User) end),
	    {[User | Users], subscribed};
	true ->
	    {Users, {error, User, already_subscribed}}
    end.

unsubscribe(User, Users) ->
    case lists:member(User, Users) of
	true ->
	    Reply = chatterbox_lib:sync_call(User, unsubscribe),
	    {Users -- [User], Reply};
	false ->
	    {Users, {error, User, not_exist}}
    end.


init_user(User) ->
    register(User, self()),
    loop_user().

loop_user() ->
    receive
	{{Ref, Pid}, unsubscribe} ->
	    Pid ! {Ref, unsubscribed}
    end.
