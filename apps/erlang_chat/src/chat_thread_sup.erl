%%%-------------------------------------------------------------------
%% @doc erlang_chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_thread_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args) ->
    {ok, {{simple_one_for_one, 5, 3600},
          [{thread,
            {chat_thread, start_link, Args},
            transient, 5000, worker,[chat_thread]}]}}.
%%====================================================================
%% Internal functions
%%====================================================================
