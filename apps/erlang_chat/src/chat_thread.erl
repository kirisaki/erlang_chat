-module(chat_thread).

-behaviour(gen_server).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name, users}).
-record(user, {name, pid, threads}).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name],[]).

init([Name]) ->
    io:format("thread ~s start", [Name]),
    {ok, #state{name=Name, users=dict:new()}}.

handle_call({user_add, User}, _From, State) ->
    Name = User#user.name,
    case dict:is_key(Name, State#state.users) of
	true ->
	    {reply, {error, already_exists}, State};
	_ ->
	    NewState = State#state{users=dict:append(Name, User, State#state.users)},
	    {reply, {ok, nop}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% inner function
