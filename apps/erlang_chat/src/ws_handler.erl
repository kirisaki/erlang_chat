-module(ws_handler).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
		{cowboy_websocket, Req, State
    , #{idle_timeout => infinity}}.

websocket_init(State) ->
    {reply, {text, << "connect." >>}, State, hibernate}.

websocket_handle(_Data, State) ->
    {ok, State, hibernate}.

websocket_info(_Info, State) ->
    {ok, State, hibernate}.
