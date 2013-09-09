-module(exportall).
-import(alfa,[start/1, kill/2]).

%%comment here
-spec hello(integer()) -> tuple().
hello(A) -> 
    {world, A}. 

-spec hello(integer()) -> tuple().
%% comment here
hello(A) -> 
    {world, A}. 

%% comment
hello(A, B) -> 
    {world, A}. 

%% @spec hello(integer(), any(), any()) -> tuple().
hello(A, B, C) -> 
    {world, A}. 
