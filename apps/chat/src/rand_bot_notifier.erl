-module(rand_bot_notifier).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-spec start_link(_) ->
    {ok, pid()}.

start_link(Chat) ->
  gen_server:start_link(?MODULE, [Chat], []).

-spec init([_]) -> {ok, #{}}.

init([Chat]) ->
  timer:send_after(3000, spam_time),
  {ok, #{chat => Chat}}.

handle_info(spam_time, #{chat := Chat} = State) ->
  pid_pool:broadcast(State, <<<<"Aviasales. The cheapest air tickets for chat ">>/binary, Chat/binary, <<"!">>/binary>>),
  RandomTime = rand:uniform(5001) + 2999,
  timer:send_after(RandomTime, spam_time),
  {noreply, State};
handle_info(Info, State) ->
  logger:alert("No air tickets info - ~p", [Info]),
  {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
  
handle_cast(_Request, State) ->
    {noreply, State}.