-module(linelength).

hello(A) -> %XxX it is 100 chars long, so it is fine, the question is what shall I write here to ex
% it only has comments, so I have to write more than previously, which is hard a little bit but I c
% this is a long line, but only contains comments, this should be marked as well, because it is a hard rule, without exceptions
    B = {world, A},
    C = {{a, b, [1,2,4,5]},{[{a, b, c, [12, 12, 34]},{[A, B], [1, 2]}], [error, error, stg, error]}},
    {error}, %this line is bigger than one hundred because of the comments, so it should be marked as ?
    {A}.
