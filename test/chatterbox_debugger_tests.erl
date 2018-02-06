-module(chatterbox_debugger_tests).
-include_lib("eunit/include/eunit.hrl").

create_debug_table_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> chatterbox_debugger:delete_table() end,
     fun() ->
             Res = chatterbox_debugger:create_table(),
             ?assertEqual(chatterbox_debugger, Res)
     end}.

delete_debug_table_test_() ->
    {setup,
     fun() -> chatterbox_debugger:create_table() end,
     fun(_) -> ok end,
     fun() ->
             chatterbox_debugger:delete_table(),
             ?assertEqual(undefined, ets:info(chatterbox_debugger))
     end}.

update_debug_table_test_() ->
    {foreach,
     fun() -> chatterbox_debugger:create_table() end,
     fun(_)-> chatterbox_debugger:delete_table() end,
     [fun initialized_when_created/0,
      fun increment_created_accounts/0,
      fun increment_logged_accounts/0,
      fun decrement_logged_accounts/0,
      fun increment_created_rooms/0,
      fun increment_failed_account_creation_attempts/0,
      fun increment_failed_room_creation_attempts/0
     ]}.

initialized_when_created() ->
    Res = lists:sort(ets:tab2list(chatterbox_debugger)),
    ?assertEqual([{number_of_created_accounts, 0},
                  {number_of_created_rooms, 0},
                  {number_of_failed_account_creation_attempts, 0},
                  {number_of_failed_room_creation_attempts, 0},
                  {number_of_logged_in_accounts, 0}
                  ],
                 Res).

increment_created_accounts() ->
    Res = chatterbox_debugger:increment_created_accounts(),
    ?assertEqual(1, Res).

increment_created_rooms() ->
    1 = chatterbox_debugger:increment_created_rooms(),
    2 = chatterbox_debugger:increment_created_rooms(),
    Res = chatterbox_debugger:increment_created_rooms(),
    ?assertEqual(3, Res).

increment_logged_accounts() ->
    1 = chatterbox_debugger:increment_logged_accounts(),
    Res = chatterbox_debugger:increment_logged_accounts(),
    ?assertEqual(2, Res).

decrement_logged_accounts() ->
    1 = chatterbox_debugger:increment_logged_accounts(),
    Res = chatterbox_debugger:decrement_logged_accounts(),
    ?assertEqual(0, Res).

increment_failed_account_creation_attempts() ->
    Res = chatterbox_debugger:increment_failed_account_creation_attempts(),
    ?assertEqual(1, Res).

increment_failed_room_creation_attempts() ->
    Res = chatterbox_debugger:increment_failed_room_creation_attempts(),
    ?assertEqual(1, Res).
