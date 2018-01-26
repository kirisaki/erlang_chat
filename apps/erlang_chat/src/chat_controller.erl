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
    ets:new(users, [set, named_table, {keypos,#user.name}, protected]),
    ets:new(threads, [set, named_table, {keypos,#thread.name}, protected]),
    self() ! {start_thread_supervisor, Sup},
    {ok, #state{}}.

handle_call({request, Raw}, {From, _Ref}, State) ->
    io:format(Raw),
    User_ = ets:select(users, [{#user{pid=From, _='_'}, [], ['$_']}]),
    [User] = case length(User_) of
                 0 -> 
                     [#user{name=""}];
                 _ ->
                     User_
             end,
    case statement_converter:decode(Raw) of
        {ok, {<<"connect">>, Name}} ->
            case ets:member(users, Name) of
                true ->
                    {reply,{<<"error">>, <<Name/binary, " is already used.">>} , State};
                _ ->   
                    ets:insert(users, #user{name=Name, pid=From, threads=[]}),
                    {reply,{<<"welcome">>, Name} , State}
            end;
        {ok, {<<"quit">>}} ->
            Name = User#user.name,
            case ets:member(users, Name) of
                true ->
                    ets:delete(users, Name),
                    {reply,{<<"goodbye">>, <<Name/binary>>} , State};
                _ ->   
                    {reply,{<<"error">>, <<Name/binary, " doesn't exist.">>} , State}
            end;
        {ok, {<<"join">>, Thread}} ->
            case ets:member(threads, Thread) of
                true ->
                    ets:update_element(users, User#user.name, [{#user.threads, [Thread|User#user.threads]}]),
                    {reply,{<<"welcome">>, <<"thread ", Thread/binary>>} , State};
                _ ->   
                    {ok, Pid} = supervisor:start_child(State#state.thread_sup, [Thread]),
                    Ref = erlang:monitor(process, Pid),
                    ets:update_element(users, User#user.name, [{#user.threads, [Thread|User#user.threads]}]),
                    ets:insert(threads, #thread{name=Thread, pid=Pid, ref=Ref}),
                    {reply,{<<"welcome">>, <<"thread ", Thread/binary, " has made.">>} , State}
            end;
        _ ->
            {reply,{<<"error">>, <<"invalide statement.">>} , State}
    end;
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
        
        
