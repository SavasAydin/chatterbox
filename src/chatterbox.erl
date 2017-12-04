-module(chatterbox).
-export([start/0,
	 stop/1]).

start() ->
    spawn(fun init/0).

stop(Pid) ->
    sync_call(Pid, stop).

sync_call(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {Ref, self(), Msg},
    receive
	{Ref, Reply} -> 
	    Reply
    after 100 ->
	    {error, timeout}
    end.	    

init() ->
    receive
	{Ref, Pid, stop} ->
	    Pid ! {Ref, stopped}
    end.
