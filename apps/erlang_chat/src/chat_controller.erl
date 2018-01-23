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
            case decode_statement(Raw) of
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
        
        
decode_statement(Binary) ->
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
            if
                Arity == length(Arguments) ->
                    {ok, list_to_tuple([list_to_binary(Statement)|Arguments])};
                true ->
                    {error}
            end;
        true -> 
            {error}
    end.

encode_statement(Tuples) ->
    case Tuples of
        {} ->
            {error};
        {<<>>} ->
            {error};
        _ ->
            [Command_|Arguments] = tuple_to_list(Tuples),
            Arity = integer_to_binary(length(Arguments)),
            Command = << Command_/binary, "/",  Arity/binary>>,
            {ok, list_to_binary(lists:join(<<" ">>, [Command|Arguments]))}
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
