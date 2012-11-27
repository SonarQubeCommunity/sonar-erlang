-module(statements).
sayHello(A) ->

    lists:foreach(fun(Name) -> call(Name, stop) end, 
		  checkpoints()), 
    Name = fun(A) -> wat(A) end,
    B = {Name, fun(Name) -> no(fun(Name) -> {Name, 2} end)+1 end}.
