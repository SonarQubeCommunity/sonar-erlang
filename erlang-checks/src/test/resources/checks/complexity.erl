-module(statements).
sayHello(A) -> 				%+1 complexity (function clause)

    lists:foreach(fun(Name) -> call(Name, stop) end, %+1 complexity fun expression --> do we need it?
		  checkpoints()), 
    Name = Cp#checkpoint_args.name, 

    if 
		LocalWriter == true -> 	%+1 complexity branch expression
		    {ok, node()};
		Name /= stop -> 	%+1 complexity branch expression
		    {ok, node()};
		Writers /= [] -> 	%+1 complexity branch expression
			{ok, hd(Writers)} 
	end,

	
	case R#retainer.really_retain of 
		true -> 		%+1 complexity pattern statement
		    PendingTab = Cp#checkpoint_args.pending_tab, 
		    case catch ?ets_lookup_element(PendingTab, Tid, 1) of 
			{'EXIT', _} ->  %+1 complexity pattern statement
			    Store = R#retainer.store 
		    end;
		false -> 		%+1 complexity pattern statement
		    ignore;
		false -> 		%+1 complexity pattern statement
		    ignore  
	    end,

	try beam_disasm:file(Name) of 
		{error,beam_lib,Reason} -> [{beam_lib,Reason}]; %+1 complexity pattern statement
		{beam_file,L} -> 				%+1 complexity pattern statement
		    {value,{code,Code0}} = lists:keysearch(code, 1, L), 
		    Code = beam_file_1(Code0, []), 
		    validate(Code) 
	    catch _:_ -> [disassembly_failed] 			%+1 complexity catch pattern statement
    end,
    
	begin 
    	start_servers()
	end.
