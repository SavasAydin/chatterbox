-module(chatterbox_lib).
-export([sync_call/2]).

sync_call(Where, Msg) ->
    Ref = make_ref(),
    Where ! {{Ref, self()}, Msg},
    receive_reply(Ref).

receive_reply(Ref) ->
    receive
	{Ref, Reply} ->
	    Reply
    after 100 ->
	    {error, timeout}
    end.
