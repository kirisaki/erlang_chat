-module(statement_converter_tests).
-include_lib("eunit/include/eunit.hrl").

statement_converter_test_() ->
    [ splitN()
    , decode()
    , encode()
    ].

splitN() ->
    {"test of splitN",
     [
      ?_assertEqual([<<"a">>, <<"b c">>], statement_converter:splitN(<<"a b c">>, <<" ">>, 1)),
      ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], statement_converter:splitN(<<"a b c">>,<<" ">>, 2)),
      ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], statement_converter:splitN(<<"a b c">>, <<" ">>, 3)),
      ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d e f">>], statement_converter:splitN(<<"a b c d e f">>, <<" ">>, 3)),
      ?_assertEqual([<<"a b c">>], statement_converter:splitN(<<"a b c">>, <<" ">>, 0)),
      ?_assertEqual([<<"string">>], statement_converter:splitN(<<"string">>, <<" ">>, 1)),
      ?_assertEqual([], statement_converter:splitN(<<"">>, <<" ">>, 1))
     ]}.

decode() ->
     {"test of decode_statement",
      [
       ?_assertEqual(
          {ok, {<<"login">>, <<"user">>, <<"pass">>}},
          statement_converter:decode(<<"login/2 user pass">>)),
       ?_assertEqual(
          {ok, {<<"ping">>}}, 
          statement_converter:decode(<<"ping/0">>)),
       ?_assertEqual(
          {error}, 
          statement_converter:decode(<<"say/2 boo">>)),
       ?_assertEqual(
          {error}, 
          statement_converter:decode(<<"foo bar">>)),
       ?_assertEqual(
          {ok, {<<"say">>, <<"123456">>, <<"nobody expects">>}},
          statement_converter:decode(<<"say/2 123456 nobody expects">>))
      ]}.
    
encode() ->
     {"test of encode",
      [
       ?_assertEqual(
          {ok, <<"login/2 user pass">>},
          statement_converter:encode({<<"login">>,<<"user">>, <<"pass">> })),
       ?_assertEqual(
          {ok, <<"pong/0">>},
          statement_converter:encode({<<"pong">>})),
       ?_assertEqual(
          {ok, <<"say/2 group quick brown fox">>},
          statement_converter:encode({<<"say">>,<<"group">>, <<"quick brown fox">> })),
       ?_assertEqual(
          {ok, <<"login/2 user pass">>},
          statement_converter:encode({<<"login">>,<<"user">>, <<"pass">> })),
       ?_assertEqual(
          {error},
          statement_converter:encode({<<>>}))
      ]}.
    
