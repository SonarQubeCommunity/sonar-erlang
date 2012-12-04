-module(statements).
-define(A,true). %simple define, should allow
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])). %ignore if setting is true
-else.
-define(LOG(X), true). %ignore if setting is true
-define(IGNOREME(X), io:format("yeah",[])).
-endif.
-define(IGNOREME(X), io:format("yeah",[])).

sayHello(A) -> 				
    Code.
-define(B,62*15+5-78).
sayBello() ->
	error.
