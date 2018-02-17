-module(chatterbox_web_sup).

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port, Protocol) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, Protocol]).

init(Args) ->
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy,
      [{chatterbox,
	{chatterbox_web_server, start, [Args]},
	permanent, 5000, worker, [chatterbox_web_server]},
       {account,
	{account_server, start_link, []},
	permanent, 1000, worker, [account_server]},
       {room,
	{room_server, start_link, []},
	permanent, 1000, worker, [room_server]}
       ]}}.
