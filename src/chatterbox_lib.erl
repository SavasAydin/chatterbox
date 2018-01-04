-module(chatterbox_lib).

-export([create_table_if_not_exist/1,
	 create_table_if_not_exist/2,
	 is_process_defined/1,
	 register_process/2,
	 to_process_name/1
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

is_process_defined(Name) ->
    whereis(to_process_name(Name)) /= undefined.

register_process(Name, Pid) ->
    register(to_process_name(Name), Pid).

to_process_name(Name) when is_list(Name) ->
    list_to_atom(Name);
to_process_name(Name) ->
    Name.
