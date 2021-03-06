%%%-------------------------------------------------------------------
%% @doc erlang_chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_chat_sup).

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
    supervisor:start_link({local, erlang_chat}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Procs =[
            { controller
            , { chat_controller
              , start_link
              , [self()]
              }
            , permanent
            , 5000
            , worker
            , dynamic
            }
           ],
    {ok, { {one_for_all, 5, 3600}, Procs} }.

%%====================================================================
%% Internal functions
%%====================================================================
