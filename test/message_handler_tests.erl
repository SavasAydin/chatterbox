-module(message_handler_tests).
-include_lib("eunit/include/eunit.hrl").

subscribe_and_unsubscribe_with_a_username_test() ->
    Username = username,
    SubReply = message_handler:subscribe(Username),
    ?assertEqual("subscribed", SubReply),
    UnsubReply = message_handler:unsubscribe(Username),
    ?assertEqual("unsubscribed", UnsubReply).

start_process_for_each_subscribed_user_test() ->
    Username = username,
    "subscribed" = message_handler:subscribe(Username),
    ?assert(erlang:is_process_alive(whereis(Username))),
    "unsubscribed" = message_handler:unsubscribe(Username).
