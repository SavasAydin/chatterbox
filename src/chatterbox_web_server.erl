-module(chatterbox_web_server).

-export([start/1]).

start([Port, Protocol]) ->
    ensure_tables(),
    M = get_module(Protocol),
    M:start(Port).

ensure_tables() ->
    chatterbox_debugger:create_table(),
    account:create_tables(),
    room:create_table().

get_module(websocket) ->
    chatterbox_websocket;
get_module(http) ->
    chatterbox_http;
get_module(_) ->
    throw("Protocol must be either websocket or http").
