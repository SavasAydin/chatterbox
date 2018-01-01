-module(message_handler).
-export([subscribe/1,
	 unsubscribe/1]).

subscribe(Username) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
		  register(Username, self()),
		  Parent ! {Ref, "subscribed"},
		  loop()
	  end),
    receive
	{Ref, Reply} ->
	    Reply
    after 100 ->
	    {error, subscribe_timeout}
    end.

unsubscribe(Username) ->
    From = self(),
    Ref = make_ref(),
    Username ! {From, Ref, unsubscribe},
    receive
	{Ref, Reply} ->
	    Reply
    after 100 ->
	    {error, unsubscribe_timeout}
    end.

loop() ->
    receive
	{Pid, Ref, unsubscribe} ->
	    Pid ! {Ref, "unsubscribed"}
    end.
