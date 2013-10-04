-module(exportall).
-compile(export_all).

-ifdef('TEST').
-compile(export_all).
-endif.

hello(A) -> 
    {world, A}. 
