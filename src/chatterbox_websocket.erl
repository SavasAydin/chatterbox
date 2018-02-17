-module(chatterbox_websocket).

-export([start/1, ws_loop/3, loop/2]).
-export([broadcast_server/1]).

start(Port) ->
    Broadcaster = spawn_link(?MODULE, broadcast_server, [dict:new()]),
    Options = [{name, client_access},
	       {loop, {?MODULE, loop, [Broadcaster]}},
	       {port, Port}],
    mochiweb_http:start_link(Options).

ws_loop(Payload, Broadcaster, _ReplyChannel) ->
    io:format("Received data: ~p~n", [Payload]),
    Received = list_to_binary(Payload),
    Broadcaster ! {broadcast, self(), Received},
    Broadcaster.

loop(Req, Broadcaster) ->
    io:format(user, "main loop~n", []),
    H = mochiweb_request:get_header_value("Upgrade", Req),
    IsConnected = H =/= undefined andalso string:to_lower(H) =:= "websocket",
    loop(Req, Broadcaster, IsConnected).

loop(Req, _, false) ->
    io:format(user, "loop false~n", []),
    mochiweb_request:serve_file([], doc_root(), Req);
loop(Req, Broadcaster, true) ->
    io:format(user, "loop true ~n", []),
    {ReentryWs, ReplyChannel} = mochiweb_websocket:upgrade_connection(
				  Req, fun ?MODULE:ws_loop/3),
    Broadcaster ! {register, self(), ReplyChannel},
    ReentryWs(Broadcaster).

broadcast_server(Pids) ->
    Pids1 = receive
		{register, Pid, Channel} ->
		    broadcast_register(Pid, Channel, Pids);
		{broadcast, Pid, Message} ->
		    broadcast_sendall(Pid, Message, Pids);
		{'DOWN', MRef, process, Pid, _Reason} ->
		    broadcast_down(Pid, MRef, Pids);
		Msg ->
		    io:format("Unknown message: ~p~n", [Msg]),
		    Pids
	    end,
    erlang:hibernate(?MODULE, broadcast_server, [Pids1]).

broadcast_register(Pid, Channel, Pids) ->
    MRef = erlang:monitor(process, Pid),
    Dict = dict:store(Pid, {Channel, MRef}, Pids),
    broadcast_sendall(Pid, "connected", Dict).

broadcast_down(Pid, MRef, Pids) ->
    NewPids = remove_pid(Pid, Pids, MRef),
    broadcast_sendall(Pid, "disconnected", NewPids).

broadcast_sendall(Pid, Msg, Pids) ->
    M = iolist_to_binary([pid_to_list(Pid), ": ", Msg]),
    dict:fold(
      fun (K, {Reply, MRef}, Acc) ->
	      try
		  begin
		      Reply(M),
		      dict:store(K, {Reply, MRef}, Acc)
		  end
	      catch
		  _:_ ->
		      Acc
	      end
      end,
      dict:new(),
      Pids).

remove_pid(Pid, Pids, MRef) ->
    case dict:find(Pid, Pids) of
	{ok, {_, MRef}} ->
	    dict:erase(Pid, Pids);
	_ ->
	    Pids
    end.

doc_root() ->
    local_path(["priv", "socket"]).

local_path(Components) ->
    {file, Loaded} = code:is_loaded(?MODULE),
    ModPath = filename:dirname(filename:dirname(Loaded)),
    filename:join([ModPath | Components]).
