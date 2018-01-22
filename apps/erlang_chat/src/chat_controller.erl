-module(chat_controller).

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
    case Raw of
        "4" ->
            {reply, ok, State};
        _ ->
            case encode_statement(Raw) of
                {ok, Command} ->
                    {reply, Command, State};
                _ ->
                    {reply, error, State}
            end
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
encode_statement(Binary) ->
    [Cmd_|Payload_] = binary:split(Binary, <<" ">>),
    Payload = case Payload_ of
                  [] ->
                      <<>>;
                  [P] ->
                      P
              end,
    Cmd = string:to_lower(binary_to_list(Cmd_)),
    [Statement|Arity_] = string:split(Cmd, "/"),
    {Arity, _} = string:to_integer(Arity_),
    if
        not (Arity == error) ->
            Arguments = splitN(Payload, <<" ">>, Arity - 1),
            Num = length(Arguments),
            if
                Arity == Num ->
                    {ok, list_to_tuple([list_to_atom(Statement)|Arguments])};
                true ->
                    {error}
            end;
        true -> 
            {error}
    end.
                           
splitN(<<>>, _, _)->
    [];
splitN(Binary, _, 0)-> 
    [Binary];
splitN(Binary, Separator, N) ->
    [Head|Tail_] = binary:split(Binary, Separator),
    case Tail_ of
        [] -> 
            [Head];
        [Tail] ->
            [Head|splitN(Tail, Separator, N-1)]
    end.
