-module(chatterbox_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Port).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Port) ->
    {ok, {{one_for_one, 3, 60},
	  [{chatterbox,
	    {chatterbox_server, start_link, [Port]},
	    permanent, 1000, worker, [chatterbox_server]},
	   {account,
	    {account_server, start_link, []},
	    permanent, 1000, worker, [account_server]},
	   {room,
	    {room_server, start_link, []},
	    permanent, 1000, worker, [room_server]}
	  ]}}.
