-module(istailrecursive).

function([H|T], Acc) ->
   case H of
       1 -> function(T, Acc+1);
       2 -> function(T, Acc-2);
       _ -> 1+function(T, Acc)
   end;

function([H|T], Acc) ->
   case H of
       1 -> function(T, Acc+1);
       2 -> function(T, Acc-2);
       _ -> function(T, Acc)
   end,
   Acc.
