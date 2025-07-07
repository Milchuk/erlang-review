-module(pid_pool).

-export([store/0, delete/0]).

-spec store() -> ok | {error, Reason :: term()}.

store() ->
    logger:alert("store pid ~p", [self()]),
    syn:join(names, names, self()).

-spec delete() -> ok | {error, Reason :: term()}.

delete() ->
    logger:alert("delete pid ~p", [self()]),
    syn:leave(names, names, self()).

