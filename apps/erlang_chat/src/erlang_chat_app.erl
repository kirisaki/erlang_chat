%%%-------------------------------------------------------------------
%% @doc erlang_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/", cowboy_static, {priv_file, websocket, "index.html"}},
                         {"/websocket", ws_handler, []},
                         {"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
                        ]
                  }
                 ]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	erlang_chat_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
