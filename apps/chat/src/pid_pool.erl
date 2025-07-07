-module(pid_pool).

-export([store/0, delete/0, broadcast/1]).

-spec store() -> ok | {error, Reason :: term()}.

store() ->
    logger:alert("store pid ~p", [self()]),
    syn:join(names, names, self()).

-spec delete() -> ok | {error, Reason :: term()}.

delete() ->
    logger:alert("delete pid ~p", [self()]),
    syn:leave(names, names, self()).

broadcast(Notification) ->
  broadcast(Notification, syn:members(names, names)).

broadcast(Notification, [{Pid, _} | Tail]) when Pid == self() ->
  broadcast(Notification, Tail);

broadcast(Notification, [{Pid, _} | Tail]) ->
  logger:alert("broadcasting to ~p", [Pid]),
  Pid ! {broadcasting, Notification},
  broadcast(Notification, Tail);

broadcast(_Notification, []) ->
  ok.