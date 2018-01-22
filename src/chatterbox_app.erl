%%%-------------------------------------------------------------------
%% @doc chatterbox public API
%% @end
%%%-------------------------------------------------------------------

-module(chatterbox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = get_port(),
    chatterbox_web_sup:start_link(Port).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
get_port() ->
    case application:get_env(chatterbox_port) of
	undefined ->
	    8080;
	{ok, Port} ->
	    Port
    end.
