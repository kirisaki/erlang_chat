-module(ws_handler).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {thread_sup}).
-record(user, {name, pid, threads}).
-record(thread, {name, pid, ref}).

init(Req0, State) ->
	NewValue = integer_to_list(rand:uniform(1000000)),
	Req1 = cowboy_req:set_resp_cookie(<<"server">>, NewValue,
                                      Req0, #{path => <<"/">>}),
	#{client := ClientCookie, server := ServerCookie}
		= cowboy_req:match_cookies([{client, [], <<>>}, {server, [], <<>>}], Req1),
    {cowboy_websocket, Req1, State, #{idle_timeout => infinity}}.

websocket_init(State) ->
    {reply, {text, << "connect." >>}, State, hibernate}.

websocket_handle({text, Msg}, State_) ->
    {reply, Response, State} = handle_request(Msg, State_),
    {ok, Statement} = statement_converter:encode(Response),
    {reply, {text, Statement}, State};
websocket_handle(_Data, State) ->
    {ok, State, hibernate}.

websocket_info(_Info, State) ->
    {ok, State}.

handle_request(Raw, State) ->
    From = self(),
    io:format(Raw),
    User_ = ets:select(users, [{#user{pid=From, _='_'}, [], ['$_']}]),
    [User] = case length(User_) of
                 0 -> 
                     [#user{name=""}];
                 _ ->
                     User_
             end,
    Statement = statement_converter:decode(Raw),
    case User#user.name of
        "" ->
            case Statement of
                {ok, {<<"connect">>, Name}} ->
                    case ets:member(users, Name) of
                        true ->
                            {reply,{<<"error">>, <<Name/binary, " is already used.">>} , State};
                        _ ->   
                            ets:insert(users, #user{name=Name, pid=From, threads=[]}),
                            {reply,{<<"welcome">>, Name} , State}
                    end;
                _ ->
                    {reply, {<<"error">>, <<"connect first!">>}, State}
            end;
        _ ->
            case Statement of
                {ok, {<<"quit">>}} ->
                    Name = User#user.name,
                    case ets:member(users, Name) of
                        true ->
                            ets:delete(users, Name),
                            {reply,{<<"goodbye">>, <<Name/binary>>} , State};
                        _ ->   
                            {reply,{<<"error">>, <<Name/binary, " doesn't exist.">>} , State}
                    end;
                {ok, {<<"join">>, Name}} ->
                    case ets:member(threads, Name) of
                        true ->
                            [Thread] = ets:select(threads, [{#thread{name=Name, _='_'}, [], ['$_']}]),
                            {reply,{<<"welcome">>, <<"thread ", Thread/binary>>} , State};
                        _ ->   
                            case gen_server:call(chat_controller, {make_thread, Name}) of
                                {ok, Thread} ->
                                    { reply
                                    , {<<"welcome">>
                                      , <<"thread ", (Thread#thread.name)/binary, " has made.">>} 
                                    , State};
                                {error, Error} ->
                                    {reply, {<<"error">>, <<"can't make thread.">>}, State}
                            end
                    end;
                _ ->
                    {reply,{<<"error">>, <<"invalide statement.">>} , State}
            end
    end.
