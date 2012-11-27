-module(refactorerl_issues).
-behaviour(supervisor).

mcCabeIssue() ->
X=1.0,
Y=2.0,
Z=1.5,
    case {X, Y, Z} of
        {0, 0, 0} -> empty_vector;    % empty_vector is an atom
        {A, A, A} -> all_the_same;    % so is all_the_same
        {X1, Y1, Z1} -> {X1 + 1, Y1 + 1, Z1 +1};
	{1, 0, 0} -> empty_vector;
	{0, 1, 0} -> empty_vector;
	{0, 0, 1} -> empty_vector;
	{1, 1, 0} -> empty_vector;
	{1, 0, 1} -> empty_vector;
	{0, 1, 1} -> empty_vector;
	{1, 1, 1} -> empty_vector;
	{1, 2, 4} -> empty_vector
    end.

maxDepthOfCalling_0() ->
   A=0,
   maxDepthOfCalling_1(A).

maxDepthOfCalling_1(A) ->
   maxDepthOfCalling_2(A+1).

maxDepthOfCalling_2(A) ->
   maxDepthOfCalling_3(A+1).

maxDepthOfCalling_3(A) ->
   maxDepthOfCalling_4(A+1).

maxDepthOfCalling_4(A) ->
   maxDepthOfCalling_5(A+1).

maxDepthOfCalling_5(A) ->
   maxDepthOfCalling_6(A+1).

maxDepthOfCalling_6(A) ->
   maxDepthOfCalling_7(A+1).

maxDepthOfCalling_7(A) ->
   maxDepthOfCalling_8(A+1).
        
maxDepthOfCalling_8(A) ->
   maxDepthOfCalling_9(A+1).

maxDepthOfCalling_9(A) ->
   maxDepthOfCalling_10(A+1).

maxDepthOfCalling_10(A) ->
   maxDepthOfCalling_11(A+1).

maxDepthOfCalling_11(A) ->
   A+1.
      
