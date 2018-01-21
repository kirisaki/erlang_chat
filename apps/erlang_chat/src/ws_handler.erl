-module(ws_handler).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
		{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    {reply, {text, << "connect." >>}, State}.

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
	{ok, State}.
