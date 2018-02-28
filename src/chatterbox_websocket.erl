-module(chatterbox_websocket).

-export([start/1, stop/0, ws_loop/3, loop/2]).
-export([broadcast_server/1, update/2]).

start(Port) ->
    Parent = self(),
    Pid = spawn(fun() ->
                        start_link(Port),
                        register(?MODULE, self()),
                        Parent ! started,
                        receive
                            stop ->
                                Parent ! ok
                        end
                end),
    receive
        started ->
            {ok, Pid}
    after 100 ->
            {error, server_start_timeout}
    end.

start_link(Port) ->
    Broadcaster = spawn_link(?MODULE, broadcast_server, [dict:new()]),
    Options = [{name, client_access},
               {loop, {?MODULE, loop, [Broadcaster]}},
               {port, Port}],
    mochiweb_http:start_link(Options).

stop() ->
    ?MODULE ! stop,
    receive
        ok ->
            ok
    after 100 ->
            {error, server_stop_timeout}
    end.

ws_loop(Payload, Broadcaster, _) ->
    io:format("Received data: ~p~n", [Payload]),
    Response = handle_request(Payload),
    io:format("Received data:      ~p~n"
              "Response generated  ~p~n",
              [Payload, Response]),
    Reply = build_json(response, Response),
    Broadcaster ! {send, self(), Reply},
    Broadcaster.

loop(Req, Broadcaster) ->
    io:format(user, "main loop~n", []),
    H = mochiweb_request:get_header_value("Upgrade", Req),
    IsConnected = H =/= undefined andalso string:to_lower(H) =:= "websocket",
    loop(Req, Broadcaster, IsConnected).

loop(Req, _, false) ->
    io:format(user, "serve file~n", []),
    mochiweb_request:serve_file([], doc_root(), Req);
loop(Req, Broadcaster, true) ->
    io:format(user, "upgrade connection~n", []),
    {ReentryWs, ReplyChannel} = mochiweb_websocket:upgrade_connection(
                                  Req, fun ?MODULE:ws_loop/3),
    Broadcaster ! {register, self(), ReplyChannel},
    ok = account_server:register_broadcaster(Broadcaster),
    ReentryWs(Broadcaster).

broadcast_server(Pids) ->
    Pids1 = receive
                {register, Pid, Channel} ->
                    broadcast_register(Pid, Channel, Pids);
                {send, Pid, Message} ->
                    broadcast_send(Pid, Message, Pids);
                {broadcast, Message} ->
                    broadcast_sendall(Message, Pids);
                {'DOWN', MRef, process, Pid, _} ->
                    broadcast_down(Pid, MRef, Pids);
                Msg ->
                    io:format("Unknown message: ~p~n", [Msg]),
                    Pids
            end,
    erlang:hibernate(?MODULE, broadcast_server, [Pids1]).

broadcast_register(Pid, Channel, Pids) ->
    MRef = erlang:monitor(process, Pid),
    Channel("connected"),
    Dict = dict:store(Pid, {Channel, MRef}, Pids),
    L = dict:to_list(Dict),
    io:format(user, "broadcast_register~nPid ~p~nDict ~p~nMsg connected~n~n", [Pid, L]),
    Dict.

broadcast_send(Pid, Msg, Pids) ->
    case dict:find(Pid, Pids) of
        {ok, {Channel, _}}->
            Channel(Msg);
        _ ->
            ok
    end,
    L = dict:to_list(Pids),
    io:format(user, "broadcast_send~nPid ~p~nDict ~p~nMsg ~p~n~n", [Pid, L, Msg]),
    Pids.

broadcast_sendall(Msg, Pids) ->
    L = dict:to_list(Pids),
    io:format(user, "broadcast_sendall~nPids ~p~nMsg ~p~n~n", [L, Msg]),
    dict:fold(
      fun (K, {Reply, MRef}, Acc) ->
              try
                  begin
                      Reply(Msg),
                      dict:store(K, {Reply, MRef}, Acc)
                  end
              catch
                  _:_ ->
                      Acc
              end
      end,
      dict:new(),
      Pids).

broadcast_down(Pid, MRef, Pids) ->
    NewPids = remove_pid(Pid, Pids, MRef),
    L = dict:to_list(NewPids),
    io:format(user, "broadcast_down~nPid ~p~nDict ~p~n~n", [Pid, L]),
    NewPids.

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

handle_request([Payload]) ->
    [M, F | T] = string:tokens(binary_to_list(Payload), " "),
    Module = list_to_atom(M),
    Fun = list_to_atom(F),
    Args = decode(T),
    Module:Fun(Args).

update(undefined, _) ->
    ok;
update(Broadcaster, Username) ->
    Msg = build_json("users", Username),
    Broadcaster ! {broadcast, Msg}.

decode(Args) ->
    Decoded = mochijson2:decode(Args, [{format, proplist}]),
    [{binary_to_list(X), binary_to_list(Y)} || {X, Y} <- Decoded].

build_json(Key, Message) ->
    Struct = {struct, [{Key, list_to_binary(Message)}]},
    mochijson2:encode(Struct).
