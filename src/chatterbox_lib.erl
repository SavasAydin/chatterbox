-module(chatterbox_lib).

-export([create_table_if_not_exist/1,
	 create_table_if_not_exist/2,
	 to_atom/1
	]).

create_table_if_not_exist(Name) ->
    create_table_if_not_exist(Name, [public, named_table]).

create_table_if_not_exist(Name, Opts) ->
    case ets:info(Name) of
	undefined ->
	    ets:new(Name, Opts);
	_ ->
	    ok
    end.

to_atom(Name) when is_list(Name) ->
    list_to_atom(Name);
to_atom(Name) ->
    Name.
