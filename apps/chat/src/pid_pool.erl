-module(pid_pool).

-export([store/1, delete/1, check_name/2, broadcast_message/2, broadcast_new_member/1, broadcast_delete_member/1]).

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

-spec broadcast_message(#{chat := _, name := _}, _) -> ok | {error, Reason :: term()}.

broadcast_message(#{chat := Chat} = __State, Notification) ->
    process_broadcast_message(Notification, syn:members(pids, Chat)).

process_broadcast_message(Notification, [{Pid, _} | Tail]) when Pid == self() ->
    process_broadcast_message(Notification, Tail);

process_broadcast_message(Notification, [{Pid, _} | Tail]) ->
  logger:alert("broadcasting to ~p", [Pid]),
  Pid ! {broadcasting_message, Notification},
  process_broadcast_message(Notification, Tail);

process_broadcast_message(_Notification, []) ->
  ok.

-spec broadcast_new_member(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

broadcast_new_member(#{chat := Chat, name := Name} = __State) ->
    process_broadcast_new_member(Name, syn:members(pids, Chat)).
  
process_broadcast_new_member(Name, [{Pid, _} | Tail]) when Pid == self() ->
    process_broadcast_new_member(Name, Tail);
  
process_broadcast_new_member(Name, [{Pid, _} | Tail]) ->
    logger:alert("broadcasting new member to ~p", [Pid]),
    Pid ! {broadcasting_new_member, Name},
    process_broadcast_new_member(Name, Tail);
  
process_broadcast_new_member(_Notification, []) ->
    ok.

-spec broadcast_delete_member(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

broadcast_delete_member(#{chat := Chat, name := Name} = __State) ->
    process_broadcast_delete_member(Name, syn:members(pids, Chat)).
      
process_broadcast_delete_member(Name, [{Pid, _} | Tail]) when Pid == self() ->
    process_broadcast_delete_member(Name, Tail);
      
process_broadcast_delete_member(Name, [{Pid, _} | Tail]) ->
    logger:alert("broadcasting delete member to ~p", [Pid]),
    Pid ! {broadcasting_delete_member, Name},
    process_broadcast_delete_member(Name, Tail);
      
process_broadcast_delete_member(_Notification, []) ->
    ok.