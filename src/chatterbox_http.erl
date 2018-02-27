-module(chatterbox_http).
-export([start/1, stop/0, loop/2]).

start(Port) ->
    DocRoot = index_file(),
    Options1 = web_specs(Port),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    Method = Req:get(method),
    [$/ | Path] = Req:get(path),
    try
	case Method of
	    'POST' ->
		[M, F] = string:tokens(Path, " "),
		Module = list_to_atom(M),
		Fun = list_to_atom(F),
		Received = Req:parse_post(),
		Response = Module:Fun(Received),
		io:format(user,
			  "Received is ~p~n"
			  "Response is ~p~n",
			  [Received, Response]),
		Reply = [{struct, [{response, list_to_binary(Response)}]}],
		Json = mochijson2:encode(Reply),
		Req:respond({200, [{"Content-Type", "application/json"}], Json});
	    _ ->
		io:format(user,
			  "GET method and path is ~p~n",
			  [Path]),
		Req:serve_file(Path, DocRoot)
	end
    catch
	Type:What ->
	    Report = ["web request failed",
		      {path, Path},
		      {type, Type}, {what, What},
		      {trace, erlang:get_stacktrace()}],
	    error_logger:error_report(Report),
	    ErrorResponse = "request failed, sorry\n",
	    Req:respond({500, [{"Content-Type", "text/plain"}], ErrorResponse})
    end.

web_specs(Port) ->
    [{ip, {0,0,0,0}},
     {port, Port}].

index_file() ->
    local_path(["priv", "www"]).

local_path(Components) ->
    {file, Loaded} = code:is_loaded(?MODULE),
    ModPath = filename:dirname(filename:dirname(Loaded)),
    filename:join([ModPath | Components]).
