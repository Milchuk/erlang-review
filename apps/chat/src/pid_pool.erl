-module(pid_pool).

-export([store/1, delete/1, check_name/2, broadcast/2]).

-spec store(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

store(#{chat := Chat, name := Name} = _State) ->
    logger:alert("store pid ~p for Chat ~p with Name ~p", [self(), Chat, Name]),
    syn:join(pids, Chat, self(), Name).

-spec delete(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

delete(#{chat := Chat} = _State) ->
    logger:alert("delete pid ~p for Chat ~p", [self(), Chat]),
    syn:leave(pids, Chat, self()).

-spec check_name(term(), term()) -> true | false.

check_name(Chat, Name) ->
    process_check(Name, syn:members(pids, Chat)).

process_check(Name, [{_, Target} | _Tail]) when Name == Target ->
    true;

process_check(Name, [{_, _Target} | Tail]) ->
    process_check(Name, Tail);

process_check(_, []) ->
    false.

-spec broadcast(#{chat := _, name := _}, _) -> ok | {error, Reason :: term()}.

broadcast(#{chat := Chat} = __State, Notification) ->
    process_broadcast(Notification, syn:members(pids, Chat)).

process_broadcast(Notification, [{Pid, _} | Tail]) when Pid == self() ->
    process_broadcast(Notification, Tail);

process_broadcast(Notification, [{Pid, _} | Tail]) ->
  logger:alert("broadcasting to ~p", [Pid]),
  Pid ! {broadcasting, Notification},
  process_broadcast(Notification, Tail);

process_broadcast(_Notification, []) ->
  ok.