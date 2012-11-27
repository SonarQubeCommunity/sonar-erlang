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
