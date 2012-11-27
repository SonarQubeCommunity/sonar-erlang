-module(statements).
sayHello(A) ->

    lists:foreach(fun(Name) -> call(Name, stop) end, %+2 statement callExpression(lists:foreach/2) and statement callExpression in fun(call/2)
		  checkpoints()), 
    Name = Cp#checkpoint_args.name, % +1 statement

    if %+1 statement (if)
		LocalWriter == true ->
		    {ok, node()}; % +1 statement
		Writers /= [] ->
			{ok, hd(Writers)} %+1 statement
	end,

	
	case R#retainer.really_retain of %+1 statement
		true ->
		    PendingTab = Cp#checkpoint_args.pending_tab, %+1 statement
		    case catch ?ets_lookup_element(PendingTab, Tid, 1) of %+1 nested case statement
			{'EXIT', _} ->
			    Store = R#retainer.store %+1 statement
		    end;
		false ->
		    ignore %+1 statement
	    end,

	try beam_disasm:file(Name) of %+2 statement try and call (beam_disasm:file/1) --> guess its wrong...
		{error,beam_lib,Reason} -> [{beam_lib,Reason}]; %+1 statement
		{beam_file,L} ->
		    {value,{code,Code0}} = lists:keysearch(code, 1, L), %+1 expression statement
		    Code = beam_file_1(Code0, []), %+1 statement
		    validate(Code) %+1 statement
	    catch _:_ -> [disassembly_failed] %+1 statement
    end,
    
	begin %+1 begin statement -->guess its wrong...
    	start_servers() %+1 statement
	end.
