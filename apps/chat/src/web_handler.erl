-module(web_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-type state() :: #{}.

-spec init(_, _) -> {cowboy_websocket, _, _}.

init(Req, _Opts) ->
    Qs = cowboy_req:parse_qs(Req),
    Name = proplists:get_value(<<"name">>, Qs, <<"no_name">>),
    Chat = proplists:get_value(<<"chat">>, Qs, <<"no_chat">>),
    {cowboy_websocket, Req, #{chat => Chat, name => Name}}.

-spec websocket_init(term()) -> {ok, state()}.

websocket_init(#{chat := Chat, name := Name} = State) ->
    case pid_pool:check_name(Chat, Name) of
        true -> 
            {stop, name_in_use};
        false ->
            pid_pool:store(State),
            pid_pool:broadcast_new_member(State),
            logger:alert("websocket_init in Chat = ~p with Name = ~p", [Chat, Name]),
            case syn:member_count(bot, Chat) of
                0 ->
                    {ok, BotPid} = rand_bot_notifier:start_link(Chat),
                    syn:join(bot, Chat, BotPid),
                    logger:alert("bot_init in Chat ~p ", [Chat]);
                1 ->
                    logger:alert("bot already init in Chat ~p ", [Chat])
            end,
            {ok, State}
    end.
    

-spec websocket_handle(_, state()) -> {reply, _, state()} | {ok, state()}.

websocket_handle({text, Text}, State) ->
    #{<<"type">> := Type, <<"data">> := Data} = jsx:decode(Text, [return_maps]),
    logger:alert("Handle message - ~p", [{Type, Data}]),
    {Reply, NewState} = case {Type, Data} of
        {<<"send_message">>, Message} ->
            Name = maps:get(name, State, <<"Noname">>),
            NamedMessage = erlang:iolist_to_binary([Name, <<": ">>, Message]),
            pid_pool:broadcast_message(State, NamedMessage),
            {{new_message, NamedMessage}, State};
        Unknown ->
            logger:alert("get unknown message - ~p", [Unknown]),
            {disconnect, State}
    end,
    {reply, {text, encode(Reply)}, NewState};
websocket_handle(_Data, State) ->
    logger:alert("get unknown message - ~p", [_Data]),
    {ok, State}.

-spec websocket_info(_, state()) -> {reply, _, state()} | {ok, state()}.

websocket_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    logger:alert("server down with reason - ~p", [Reason]),
    {reply, {text, encode({service_message, <<"Server down">>})}, State};
websocket_info({broadcasting_message, Notification}, State) ->
    {reply, {text, encode({new_message, Notification})}, State};
websocket_info({broadcasting_new_member, Notification}, State) ->
    {reply, {text, encode({new_member, Notification})}, State};
websocket_info(Info, State) ->
    logger:alert("Get unexpected info - ~p", [Info]),
    {ok, State}.

encode({Type, Data}) ->
    jsx:encode(#{type => Type, data => Data}).

-spec terminate(_, _, state()) -> ok.

terminate(_Reason, _PartialReq, State) ->
    pid_pool:delete(State),
    ok.
