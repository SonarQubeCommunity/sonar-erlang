-module(export_one_function_per_line).
-export([start/0, start/2]). %Good
-export([stop/0, kill/0]). %not good
-export([log/2, %good
	log/3, %not good
         list/2, %good
	 update/2, refresh/2 %not good
        ]).

hello(A) -> 
    {world, A}. 
