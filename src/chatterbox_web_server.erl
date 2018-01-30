-module(chatterbox_web_server).
-export([start/1, stop/0, loop/2]).

start(Options) ->
    ensure_tables(),
    {DocRoot, Options1} = get_option(docroot, Options),
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

ensure_tables() ->
    chatterbox_debugger:create_table(),
    account:create_table(),
    room:create_table().

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
