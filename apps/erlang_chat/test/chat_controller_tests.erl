-module(chat_controller_tests).
-include_lib("eunit/include/eunit.hrl").

chat_controller_test_()->
    [ splitN()
    , decode_statement()
    , encode_statement()
    ].

splitN() ->
    {"test of splitN",
     [
      ?_assertEqual([<<"a">>, <<"b c">>], chat_controller:splitN(<<"a b c">>, <<" ">>, 1)),
      ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], chat_controller:splitN(<<"a b c">>,<<" ">>, 2)),
      ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], chat_controller:splitN(<<"a b c">>, <<" ">>, 3)),
      ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d e f">>], chat_controller:splitN(<<"a b c d e f">>, <<" ">>, 3)),
      ?_assertEqual([<<"a b c">>], chat_controller:splitN(<<"a b c">>, <<" ">>, 0)),
      ?_assertEqual([<<"string">>], chat_controller:splitN(<<"string">>, <<" ">>, 1)),
      ?_assertEqual([], chat_controller:splitN(<<"">>, <<" ">>, 1))
     ]}.

decode_statement() ->
     {"test of decode_statement",
      [
       ?_assertEqual(
          {ok, {<<"login">>, <<"user">>, <<"pass">>}},
          chat_controller:decode_statement(<<"login/2 user pass">>)),
       ?_assertEqual(
          {ok, {<<"ping">>}}, 
          chat_controller:decode_statement(<<"ping/0">>)),
       ?_assertEqual(
          {error}, 
          chat_controller:decode_statement(<<"say/2 boo">>)),
       ?_assertEqual(
          {error}, 
          chat_controller:decode_statement(<<"foo bar">>)),
       ?_assertEqual(
          {ok, {<<"say">>, <<"123456">>, <<"nobody expects">>}},
          chat_controller:decode_statement(<<"say/2 123456 nobody expects">>))
      ]}.
    
encode_statement() ->
     {"test of encode_statement",
      [
       ?_assertEqual(
          {ok, <<"login/2 user pass">>},
          chat_controller:encode_statement({<<"login">>,<<"user">>, <<"pass">> })),
       ?_assertEqual(
          {ok, <<"pong/0">>},
          chat_controller:encode_statement({<<"pong">>})),
       ?_assertEqual(
          {ok, <<"say/2 group quick brown fox">>},
          chat_controller:encode_statement({<<"say">>,<<"group">>, <<"quick brown fox">> })),
       ?_assertEqual(
          {ok, <<"login/2 user pass">>},
          chat_controller:encode_statement({<<"login">>,<<"user">>, <<"pass">> })),
       ?_assertEqual(
          {error},
          chat_controller:encode_statement({<<>>}))
      ]}.
    
