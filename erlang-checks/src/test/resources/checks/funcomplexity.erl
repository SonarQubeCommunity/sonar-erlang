-module(statements).
sayHello(A) -> 				
        Name = Cp#checkpoint_args.name, 
	fun(Name, A) -> 
	    lists:foreach(
		fun(Name) -> 
			case Name of
				hello -> call(Name, stop);
				error -> call(Name, start)
			end
		end, 
		checkpoints()), 
	    if 
		LocalWriter == true -> 	
		    {ok, node()};
		Name /= stop -> 	
		    {ok, node()};
		Writers /= [] -> 	
			{ok, hd(Writers)} 
	    end
	end.
