-module(statements).
-define(A,true).
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])). %ignore if setting is true
-else.
-define(LOG(X), true). %ignore if setting is true
-endif.
sayHello(A) -> 				
    Code.
-define(B,false).
sayBello() ->
	error.
