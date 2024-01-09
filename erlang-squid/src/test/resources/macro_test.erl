-module(my_type).

-spec my_field(my_type:my_type()) -> atom() | tuple().

my_field(MyType) ->
    (my_type:my_type(MyType))#?MY_TYPE.my_field.
