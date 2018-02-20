-module(chatterbox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, change/0, change/1]).

%%--------------------------------------------------------------------
start(_, _) ->
    Port = get_port(),
    Protocol = get_protocol(),
    chatterbox_web_sup:start_link(Port, Protocol).

%%--------------------------------------------------------------------
stop(_) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
get_port() ->
    case application:get_env(chatterbox, port) of
        undefined ->
            8080;
        {ok, Port} ->
            Port
    end.

get_protocol() ->
    case application:get_env(chatterbox, protocol) of
        undefined ->
            websocket;
        {ok, Protocol} ->
            Protocol
    end.

change() ->
    case get_protocol() of
        websocket ->
            change(http);
        http ->
            change(websocket)
    end.

change(Protocol) ->
    ok = application:stop(chatterbox),
    ok = application:set_env(chatterbox, protocol, Protocol),
    application:start(chatterbox).
