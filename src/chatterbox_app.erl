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
    case application:get_env(chatterbox_port) of
	undefined ->
	    chatterbox_sup:start_link(12345);
	{ok, Port} ->
	    chatterbox_sup:start_link(Port)
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
