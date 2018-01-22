-module(chatterbox_web_server).
-export([start/1, stop/0, loop/2]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    Res = mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]),
    io:format(user, 
              "~n~n**************************************************~n"
              "mochiweb_http:start/1 returns ~p~n",
              [Res]),
    Res.

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    io:format(user,
              "DocRoot is ~p~n"
              "Req is ~p~n",
             [DocRoot, Req]),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                  "hello_world" ->
                    Req:respond({200, [{"Content-Type", "text/plain"}],
                    "Hello world!\n"});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            io:format(user,
                      "Web request failed due to ~p:~p~n",
                      [Type, What]),

            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
