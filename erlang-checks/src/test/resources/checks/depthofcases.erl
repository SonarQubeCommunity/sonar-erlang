-module(statements).
sayHello(A) -> 				%+1 complexity (function clause)

   case X of
        1 -> true;
        2 -> case A of 
            1 -> true;
            2 -> case A of 
                1 -> true;
                2 -> case A of 
                    1 -> true;
                    2 -> case A of 
                        1 -> true;
                        2 -> case A of 
                            1 -> true;
                            2 -> false
                            end
                        end
                    end
                end
            end
   end,
   case A of 
       1 -> true;
       2 -> case A of 
           1 -> true;
           2 -> case A of 
               1 -> true;
               2 -> case A of 
                   1 -> true;
                   2 -> false
               end
           end
       end
    end.
