%%%-------------------------------------------------------------------
%%% @author danirukun
%%% @doc
%%% Test suite for erlcount_lib
%%% @end
%%%-------------------------------------------------------------------

-module(erlcount_lib_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, suite/0]). %% CT callbacks
-export([test_find_erl/1, test_regex_count/1]).

all() ->
    [test_find_erl, test_regex_count].

init_per_suite(Config) ->
    prepare_erl_files(),
    Config.

end_per_suite(_Config) ->
    clean_erl_files().

suite() ->
    [{timetrap, {seconds, 30}}].

test_find_erl(_Config) ->
    Exp = lists:sort(
              string:tokens(
                  os:cmd("find test-files -name *.erl"), "\n")),

    Exp = lists:sort(build_list(erlcount_lib:find_erl("test-files"))).

test_regex_count(_Config) ->
    5 = erlcount_lib:regex_count("a", "a a a a a"),
    0 = erlcount_lib:regex_count("o", "a a a a a"),
    2 = erlcount_lib:regex_count("a.*", "a a a\na a a"),
    3 =
        erlcount_lib:regex_count("if",
                                 "myiffun() ->\n if 1 < \" if \" -> ok;\n    true -> other\n "
                                 "end.\n"),
    1 =
        erlcount_lib:regex_count("if[\\s]{1}(?:.+)->",
                                 "myiffun() ->\n if 1 < \" if  \" -> ok;\n    true -> other\n "
                                 "end.\n"),
    2 =
        erlcount_lib:regex_count("if[\\s]{1}(?:.+)->",
                                 "myiffun() ->\n if 1 < \" if  \" -> ok;\n    true -> other\n "
                                 "end,\n if true -> ok end.\n").

%%% HELPERS

build_list(Term) ->
    build_list(Term, []).

build_list(done, List) ->
    List;
build_list({continue, Entry, Fun}, List) ->
    build_list(Fun(), [Entry | List]).

prepare_erl_files() ->
    ct:pal("Creating test file dir for temp erl source files"),
    os:cmd("mkdir test-files && cd test-files && touch erlc{001..003}.erl").

clean_erl_files() ->
    os:cmd("rm -rf test-files").
