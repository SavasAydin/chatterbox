-module(chatterbox_debugger).
-export([create_table/0,
         delete_table/0,
         initialize/0,
         increment_created_accounts/0,
         increment_logged_accounts/0,
         decrement_logged_accounts/0,
         increment_created_rooms/0,
         increment_failed_account_creation_attempts/0,
         increment_failed_room_creation_attempts/0,
         collect_logs/0
        ]).

create_table() ->
    Res = chatterbox_lib:create_table_if_not_exist(?MODULE),
    true = initialize(),
    Res.

delete_table() ->
    chatterbox_lib:delete_table_if_exists(?MODULE).

initialize() ->
    Keys = [number_of_created_accounts,
            number_of_created_rooms,
            number_of_logged_in_accounts,
            number_of_failed_account_creation_attempts,
            number_of_failed_room_creation_attempts],
    ets:insert(?MODULE, [{K, 0} || K <- Keys]).

increment_created_accounts() ->
    Key = number_of_created_accounts,
    ets:update_counter(?MODULE, Key, {2, 1}).

increment_created_rooms() ->
    Key = number_of_created_rooms,
    ets:update_counter(?MODULE, Key, {2, 1}).

increment_logged_accounts() ->
    Key = number_of_logged_in_accounts,
    ets:update_counter(?MODULE, Key, {2, 1}).

decrement_logged_accounts() ->
    Key = number_of_logged_in_accounts,
    ets:update_counter(?MODULE, Key, {2, -1}).

increment_failed_account_creation_attempts() ->
    Key = number_of_failed_account_creation_attempts,
    ets:update_counter(?MODULE, Key, {2, 1}).

increment_failed_room_creation_attempts() ->
    Key = number_of_failed_room_creation_attempts,
    ets:update_counter(?MODULE, Key, {2, 1}).

collect_logs() ->
    Logins = collect_login_data(),
    Counters = collect_debug_counters(),
    io:format("### Logged in users ###~n~s~n"
              "### Counters ###~n~s~n",
              [lists:flatten(Logins),
               lists:flatten(Counters)]).

collect_login_data() ->
    LoginText = "~p logged in since ~p~n",
    collect(logins, LoginText).

collect_debug_counters() ->
    CounterText = "~p are ~p~n",
    collect(chatterbox_debugger, CounterText).

collect(Table, Text) ->
    Entries = ets:tab2list(Table),
    [io_lib:format(Text, [Key, Value]) || {Key, Value} <- Entries].
