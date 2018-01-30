-module(chat_controller).

-behaviour(gen_server).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SUP_SPEC,
        {thread_sup,
         {chat_thread_sup, start_link, []},
          permanent,
          10000,
          supervisor,
          [chat_thread_sup]}).

-record(state, {thread_sup}).
-record(user, {name, pid, threads}).
-record(thread, {name, pid, ref}).

start_link(Sup) ->
    gen_server:start_link({local, chat_controller}, ?MODULE, {Sup}, []).

init({Sup}) ->
    ets:new(users, [set, named_table, {keypos,#user.name}, public]),
    ets:new(threads, [set, named_table, {keypos,#thread.name}, public]),
    self() ! {start_thread_supervisor, Sup},
    {ok, #state{}}.

handle_call({make_thread, Name}, _From, State) ->
    case ets:member(threads, Name) of
        true ->
            {reply, {error, already_exists}, State};
        _ ->
            case supervisor:start_child(State#state.thread_sup, [Name]) of
                {ok, Pid} ->
                    Ref = erlang:monitor(process, Pid),
                    Thread = #thread{name=Name, pid=Pid, ref=Ref},
                    ets:insert(threads, Thread),
                    {reply, {ok, Thread}, State};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end
    end;
handle_call({threads}, _From, State) ->
    Threads = ets:select(threads,[{ #thread{name='$1', _ = '_'}, [], ['$1']}]),
    {reply, {ok, Threads}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_thread_supervisor, Sup}, State) ->
    Res = supervisor:start_child(Sup, ?SUP_SPEC),
    case Res of
        {ok, Pid} ->
            {noreply, State#state{thread_sup = Pid}};
        {_, Error} ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% inner function
        
        
