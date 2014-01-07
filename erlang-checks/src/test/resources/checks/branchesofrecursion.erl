-module(a).
quicksort([H|T]) ->
    {Smaller_Ones,Larger_Ones} = a:split(H,T,{[],[]}),
    lists:append( quicksort(Smaller_Ones),
                  [H | quicksort(Larger_Ones)]
                );
quicksort([]) -> [].

split(Pivot, [H|T], {Acc_S, Acc_L}) ->
    if Pivot > H -> New_Acc = { [H|Acc_S] , Acc_L };
       true      -> New_Acc = { Acc_S , [H|Acc_L] }
    end,
    split(Pivot,T,New_Acc);
split(_,[],Acc) -> Acc.

acc_multipart(V) ->
	acc_multipart((parser(<<"boundary">>))(V), []).

acc_multipart({headers, Headers, Cont}, Acc) ->
	acc_multipart(Cont(), [{Headers, []}|Acc]);
acc_multipart({body, Body, Cont}, [{Headers, BodyAcc}|Acc]) ->
	acc_multipart(Cont(), [{Headers, [Body|BodyAcc]}|Acc]);
acc_multipart({end_of_part, Cont}, [{Headers, BodyAcc}|Acc]) ->
	Body = list_to_binary(lists:reverse(BodyAcc)),
	acc_multipart(Cont(), [{Headers, Body}|Acc]);
acc_multipart(eof, Acc) ->
	lists:reverse(Acc).
