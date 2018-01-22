-module(chatterbox_web_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Port).

init(Port) ->
    Web = web_specs(chatterbox_web_server, Port),
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, 
      [Web,
       {account,
        {account_server, start_link, []},
        permanent, 1000, worker, [account_server]},
       {room,
        {room_server, start_link, []},
        permanent, 1000, worker, [room_server]}
       ]}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.

local_path(Components) ->
    {file, Loaded} = code:is_loaded(?MODULE),
    ModPath = filename:dirname(filename:dirname(Loaded)),
    filename:join([ModPath | Components]).
    
