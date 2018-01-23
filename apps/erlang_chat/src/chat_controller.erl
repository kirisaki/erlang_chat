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

-record(state, {sup, refs}).

start_link(Sup) ->
    gen_server:start_link({local, chat_controller}, ?MODULE, {Sup}, []).

init({Sup}) ->
    self() ! {start_thread_supervisor, Sup},
    {ok, #state{refs=gb_sets:empty()}}.

handle_call({request, Raw}, {From, _Ref}, State) ->
    case Raw of
        "4" ->
            {reply, ok, State};
        _ ->
            case statement_converter:decode(Raw) of
                {ok, Statement} ->
                    gen_server:cast(?MODULE, Statement),
                    {reply, Statement, State};
                _ ->
                    {reply, error, State}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({<<"make_thread">>, Id, Name}, State) ->
    Res = supervisor:start_child(State#state.sup,[Name, Id]),
    case Res of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {noreply, State#state{refs=gb_sets:add(Ref, State#state.refs)}};
        Error ->
            error(Error),
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_thread_supervisor, Sup}, State) ->
    Res = supervisor:start_child(Sup, ?SUP_SPEC),
    case Res of
        {ok, Pid} ->
            {noreply, State#state{sup = Pid}};
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
        
        
