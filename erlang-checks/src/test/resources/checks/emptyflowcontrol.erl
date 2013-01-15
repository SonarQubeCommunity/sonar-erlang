-module(exportall).
-import(alfa,[start/1, kill/2]).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
%-define(LOG(X), true).
-endif.

-ifdef(test).
%-export([]).
-else.
-export([hello/1]).
-endif.

-ifdef(world).
-else.
-endif.

hello(A) -> 
    {world, A}. 
