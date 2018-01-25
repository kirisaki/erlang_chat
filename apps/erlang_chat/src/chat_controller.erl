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

-record(state, {sup, threads, users}).

start_link(Sup) ->
    gen_server:start_link({local, chat_controller}, ?MODULE, {Sup}, []).

init({Sup}) ->
    self() ! {start_thread_supervisor, Sup},
    {ok, #state{threads=dict:new(), users=dict:new()}}.

handle_call({request, Raw}, {From, _Ref}, State) ->
    io:format(Raw),
    case statement_converter:decode(Raw) of
        {ok, {<<"connect">>, Name}} ->
            case dict:is_key(Name, State#state.users) of
                true ->
                    {reply,{<<"error">>, <<Name/binary, " is already used.">>} , State};
                _ ->   
                    NewState = #state{users=dict:append(Name, From, State#state.users)},
                    {reply,{<<"welcome">>, Name} , NewState}
            end;
        {ok, {<<"quit">>, Name}} ->
            case dict:is_key(Name, State#state.users) of
                true ->
                    NewState = #state{users=dict:erase(Name, State#state.users)},
                    {reply,{<<"goodbye">>, <<Name/binary>>} , NewState};
                _ ->   
                    {reply,{<<"error">>, <<Name/binary, "doesn't exist.">>} , State}
            end;
        {ok, {<<"join">>, Name}} ->
            case dict:is_key(Name, State#state.users) of
                true ->
                    NewState = #state{users=dict:erase(Name, State#state.users)},
                    {reply,{<<"goodbye">>, <<Name/binary>>} , NewState};
                _ ->   
                    {reply,{<<"error">>, <<Name/binary, " doesn't exist.">>} , State}
            end;
        _ ->
            {reply,{<<"error">>, <<"invalide statement.">>} , State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({<<"nick">>, Id, Name}, State) ->
    Res = supervisor:start_child(State#state.sup,[Name, Id]),
    case Res of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {noreply, State};
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
        
        
