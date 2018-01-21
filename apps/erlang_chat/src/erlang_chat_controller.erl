-module(erlang_chat_controller).

-behaviour(gen_server).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({request, Raw}, {From, _Ref}, State) ->
    case parse_command(Raw) of
        {ok, Command} ->
            {reply, Command, State};
        _ ->
            {reply, error, State}
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
parse_command(String) ->
    [Cmd_|Payload] = string:split(String, " "),
    Cmd = string:to_lower(Cmd_),
    Statements =     [ "login/1"
                     , "group/1"
                     , "say/2"
                     ],
    Exists = lists:any(fun(Elm) -> Cmd == Elm end, Statements),
    [Statement|Arity_] = string:split(Cmd, "/"),
    {Arity, _} = string:to_integer(Arity_),
    %{ok, {Exists, Cmd, Statement, Arity_,Arity}}.
    if
        Exists and not (Arity == error) ->
            Arguments = splitN(Payload, " ", Arity),
            {ok, list_to_tuple([list_to_atom(Statement)|Arguments])};
        true -> 
            {error}
    end.
                           

splitN(_, _, 0)-> 
    [];
splitN(String, Separator, N) ->
    [Head|Tail] = string:split(String, Separator),
    [Head|splitN(Tail, Separator, N-1)].
