-module(statement_converter).

-export([decode/1, encode/1]).

decode(Binary) ->
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

encode(Tuples) ->
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
