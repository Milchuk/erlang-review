-module(pid_pool).

-export([store/1, delete/1]).
-export([check_name/2]).
-export([broadcast_message/2, broadcast_new_member/1, broadcast_delete_member/1, send_members/1, update_member/1, send_members_self/1]).

%% @doc Добавляет pid в группу чата
-spec store(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

store(#{chat := Chat, name := Name} = _State) ->
    logger:alert("store pid ~p for Chat ~p with Name ~p", [self(), Chat, Name]),
    syn:join(pids, Chat, self(), Name).

%% @doc Удаляет pid из группы чата
-spec delete(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

delete(#{chat := Chat} = _State) ->
    logger:alert("delete pid ~p for Chat ~p", [self(), Chat]),
    syn:leave(pids, Chat, self()).

%% @doc Проверяет по метаданным группы чата есть ли такое имя
-spec check_name(term(), term()) -> true | false.

check_name(Chat, Name) ->
    process_check(Name, syn:members(pids, Chat)).

process_check(Name, [{_, Target} | _Tail]) when Name == Target ->
    true;

process_check(Name, [{_, _Target} | Tail]) ->
    process_check(Name, Tail);

process_check(_, []) ->
    false.

%% @doc Отправляет сообщение всем участникам чата кроме себя
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

%% @doc Отправляет сообщение о новом участнике всем участникам чата кроме себя
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

%% @doc Отправляет сообщение о уходе участника всем участникам чата кроме себя (очищает список участников клиента)
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

%% @doc Отправляет всем в чате имя для списка участников
-spec update_member(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

update_member(#{chat := Chat, name := Name}) ->
    process_update_member(Name, syn:members(pids, Chat)).
      
process_update_member(Name, [{Pid, _} | Tail]) ->
    logger:alert("updating member to ~p", [Pid]),
    Pid ! {update_member, Name},
    process_update_member(Name, Tail);
      
process_update_member(_Notification, []) ->
    ok.

%% @doc Отправляет всем в чате все активные имена для списка участников
-spec send_members(#{chat := _, name := _}) -> ok | {error, Reason :: term()}.

send_members(#{chat := Chat}) ->
    process_send_members(Chat, syn:members(pids, Chat)).

process_send_members(Chat, [{_, Target} | Tail]) ->
    update_member(#{chat => Chat, name => Target}),
    process_send_members(Chat, Tail);

process_send_members(_Name, []) ->
    ok.

%% @doc Отправляет активные имена для списка участников только себе
-spec send_members_self(#{chat := _}) -> ok | {error, Reason :: term()}.

send_members_self(#{chat := Chat}) ->
    process_send_members_self(Chat, syn:members(pids, Chat)).

process_send_members_self(Chat, [{_, Target} | Tail]) ->
    self() ! {update_member, Target},
    process_send_members_self(Chat, Tail);

process_send_members_self(_Chat, []) ->
    ok.