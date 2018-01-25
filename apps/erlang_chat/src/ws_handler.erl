-module(ws_handler).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req0, State) ->
	NewValue = integer_to_list(rand:uniform(1000000)),
	Req1 = cowboy_req:set_resp_cookie(<<"server">>, NewValue,
                                      Req0, #{path => <<"/">>}),
	#{client := ClientCookie, server := ServerCookie}
		= cowboy_req:match_cookies([{client, [], <<>>}, {server, [], <<>>}], Req1),
    {cowboy_websocket, Req1, State, #{idle_timeout => infinity}}.

websocket_init(State) ->
    {reply, {text, << "connect." >>}, State, hibernate}.

websocket_handle({text, Msg}, State) ->
    Response = gen_server:call(chat_controller, {request, Msg}),
    {ok, Statement} = statement_converter:encode(Response),
    {reply, {text, Statement}, State};
websocket_handle(_Data, State) ->
    {ok, State, hibernate}.

websocket_info(_Info, State) ->
    {ok, State}.
