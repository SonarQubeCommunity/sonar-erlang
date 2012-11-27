-module(a).
-export(int/0).

-spec int(value()) -> ConvertedValue::term().
int(Var) -> to_type(int, Var).

-spec hello() -> atom().
hello()->
	Code;
hello(A) ->
    Code.
bello()-> %issue
    code;
bello(A) ->
    {A, code}.

%not issue
%not issue
hello2()->
	Code;

hello2(A) -> %issue
    Code.


bello2()-> %issue
    code;
%not issue
bello2(A) ->
    {A, code}.
