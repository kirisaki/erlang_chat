-module(chat_controller_tests).
-include_lib("eunit/include/eunit.hrl").

splitN_test_() ->
    [
     ?_assertEqual([<<"a">>, <<"b c">>], chat_controller:splitN(<<"a b c">>, <<" ">>, 1)),
     ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], chat_controller:splitN(<<"a b c">>,<<" ">>, 2)),
     ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], chat_controller:splitN(<<"a b c">>, <<" ">>, 3)),
     ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d e f">>], chat_controller:splitN(<<"a b c d e f">>, <<" ">>, 3)),
     ?_assertEqual([<<"a b c">>], chat_controller:splitN(<<"a b c">>, <<" ">>, 0)),
     ?_assertEqual([<<"string">>], chat_controller:splitN(<<"string">>, <<" ">>, 1)),
     ?_assertEqual([], chat_controller:splitN(<<"">>, <<" ">>, 1))
    ].

encode_statement_test_() ->
    [
     ?_assertEqual(
        {ok, {login, <<"user">>, <<"pass">>}},
        chat_controller:encode_statement(<<"login/2 user pass">>)),
     ?_assertEqual(
        {ok, {ping}}, 
        chat_controller:encode_statement(<<"ping/0">>)),
     ?_assertEqual(
        {error}, 
        chat_controller:encode_statement(<<"say/2 boo">>)),
     ?_assertEqual(
        {error}, 
        chat_controller:encode_statement(<<"foo bar">>)),
     ?_assertEqual(
        {ok, {say, <<"123456">>, <<"nobody expects">>}},
        chat_controller:encode_statement(<<"say/2 123456 nobody expects">>))
    ].
    
