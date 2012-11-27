-module(erlcount_sup).
-compile(export_all).
-behaviour(supervisor).


%%------------------
%% VERY IMPORTANT COMMENT
%%===================
start_link() ->
    supervisor:start_link(?MODULE, []).

%Simple comment
init([]) ->
    MaxRestart = 5, %inline comment
    MaxTime = 100,
	%%Another simple comment
    {ok, {{one_for_one, MaxRestart, MaxTime},
     [{dispatch,
       {erlcount_dispatch, start_link, []}, %%another inline comment
        transient,
        60000,
        worker,
        [erlcount_dispatch]}]}}.
        
%% Specification defined, but it is also commented
-spec erlcount_sup:minus2(integer()) -> integer(). %TODO
minus2(X) ->
    X-2.

-spec add(integer(),integer()) -> integer(). %ToDo
add(Q, Z) ->
   minus2(Q+Z).    
