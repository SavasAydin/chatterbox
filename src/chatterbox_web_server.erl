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
    Path = Req:get(path),
    case Path of
	"/create_account" ->
	    Reply = [{struct, [{string, <<"account is created">>}]}],
	    Json = mochijson2:encode(Reply),
	    Req:respond({200, [{"Content-Type", "application/json"}], Json});
	_ ->
	    loop2(Req, DocRoot)
    end.

loop2(Req, DocRoot) ->
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
			io:format(user,
				  "GET method and path is ~p~n",
				  [Path]),
			Req:respond({200, [{"Content-Type", "text/plain"}],
				     "Hello world!\n"});
                  "create_account" ->
			io:format(user,
				  "GET method and path is create_account~n",[]),
			Req:respond({200, [{"Content-Type", "text/plain"}],
				     "Created!\n"});

                    Other ->
			io:format(user,
				  "GET method and path is ~p~n",
				  [Other]),
			
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
			io:format(user,
				  "POST method and path is ~p~n",
				  [Path]),
                        Req:not_found()
                end;
            OtherMethod ->
		io:format(user,
			  "method is ~p~n",
			  [OtherMethod]),
		
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
