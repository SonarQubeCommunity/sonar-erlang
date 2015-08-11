-module(macro_with_record_field).

-define(MACRO, my_macro).

-record(?MACRO, {
  field
}).

my_function() ->
  ets:new(?MACRO, [set, named_table, public, {keypos, #?MACRO.field}, {read_concurrency, true}]).
