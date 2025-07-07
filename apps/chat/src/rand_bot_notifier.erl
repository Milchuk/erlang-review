-module(rand_bot_notifier).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  timer:send_after(3000, spam_time),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(spam_time, State) ->
  pid_pool:broadcast(<<"Aviasales. The cheapest air tickets!">>),
  RandomTime = rand:uniform(5001) + 2999,
  timer:send_after(RandomTime, spam_time),
  {noreply, State};
handle_info(Info, State) ->
  logger:alert("No air tickets info - ~p", [Info]),
  {noreply, State}.

