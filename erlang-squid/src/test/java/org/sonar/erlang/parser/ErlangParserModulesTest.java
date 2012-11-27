/*
 * Sonar Erlang Plugin
 * Copyright (C) 2012 Tamas Kende
 * kende.tamas@gmail.com
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.sonar.erlang.parser;

import org.sonar.erlang.ErlangConfiguration;
import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.parser.ErlangParser;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ExtendedStackTrace;
import com.sonar.sslr.impl.events.ExtendedStackTraceStream;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserModulesTest {
  ExtendedStackTrace listener = new ExtendedStackTrace();
  Parser<ErlangGrammar> p = ErlangParser
      .create(new ErlangConfiguration(Charsets.UTF_8), listener);

  ErlangGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.getRootRule());
  }

  @Test
  public void realLife() {
    assertThat(p).matches(
        (code("-module(m).", "-export([fact/1]).", "", "fact(N) when N>0 ->",
            "N * fact(N-1);", "fact(0) ->", "1.")));
  }

  @Test
  public void tuple() throws IOException, URISyntaxException {
    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "{a, node()}.")));
  }

  @Test
  public void returnWithNumber() throws IOException, URISyntaxException {
    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "1.")));
  }

  @Test
  public void returnWithCalc() throws IOException, URISyntaxException {
    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "{a, A + 2}.")));
  }

  @Test
  public void returnWithCalcCase() throws IOException, URISyntaxException {
    assertThat(p)
        .matches(
            (code("-module(m).", "dodo(A) ->", "case A of", "0->",
                "{a, (A + 2), <<0>>} end.")));
  }

  @Test
  public void caseTuple() throws IOException, URISyntaxException {
    assertThat(p).matches(
        (code("-module(m).", "dodo(A) ->", "case A of",
            "{aborted, {already_exists, user}} -> ok end.")));
    assertThat(p).matches(
        (code("-module(m).", "dodo(A) ->", "case A of",
            "{atomic, ok} -> init_user_data();",
            " {aborted, {already_exists, user}} -> ok end.")));
  }

  @Test
  public void deepFuncArg() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "dodo(A) ->",
                "io:format(\"~s~n\",[agner_spec:property_to_list(lists:keyfind(list_to_atom(Property), 1, Spec))]).")));
  }

  @Test
  public void booleanReturn() {
    assertThat(p).matches(
        (code("-module(m).", "dodo(A) ->",
            "string:rstr(Searchable, string:to_lower(Search)) > 0.")));
  }

  @Test
  public void deepFuncArg2() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "dodo(A) ->",
                "io:format(\"~s~n\",fun (Name) ->"
                  + "Spec = agner:spec(Name),"
                  + "Searchable = string:to_lower(lists:flatten([Name,proplists:get_value(description,Spec,[])|proplists:get_value(keywords,Spec,[])]))"
                  + "end).")));
  }

  @Test
  public void deepArithmetic() {
    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "2*4.")));

    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "((2)+3)+4*((3+2)*4).")));
    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "((A bsr 4) band 16#f).")));

  }

  @Test
  public void custom() {
    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "Spec = agner:spec(Name).")));
  }

  @Test
  public void noArgFunction() {
    assertThat(p).matches((code("-module(m).", "dodo() ->", "{maci}.")));
  }

  @Test
  public void functionDeclaration() {
    assertThat(p).matches((code("-module(m).", "method(A) -> a.")));
    assertThat(p).matches((code("-module(m).", "method(A)->b+2; method(_,V) -> true.")));
    assertThat(p).matches((code("-module(m).", "method(A) when A+5 >=12 -> a.")));
    assertThat(p)
        .matches(
            (code("-module(m).",
                "%% Send the last chunk of a fragmented command.",
                "packet_fragment_send(#client{socket=Socket, transport=Transport}, Packet,",
                "Size, Current) when Size - Current =< 16#4000 ->",
                "FragmentSize = 16#10 + byte_size(Packet),",
                "Fragment = << FragmentSize:32/little, 16#0b030000:32, Size:32/little, Current:32/little, Packet/binary >>,",
                "Transport:send(Socket, Fragment);",
                "%% Send another chunk of a fragmented command.",
                "packet_fragment_send(Client=#client{socket=Socket, transport=Transport}, Packet,",
                "Size, Current) ->",
                "<< Chunk:131072/bits, Rest/bits >> = Packet,",
                "Fragment = << 16#10400000:32, 16#0b030000:32, Size:32/little, Current:32/little, Chunk/binary >>,",
                "Transport:send(Socket, Fragment),",
                "packet_fragment_send(Client, Rest, Size, Current + 16#4000).")));

    assertThat(p).matches(
        (code("-module(m).", "packet_prepare(Packet) ->", "Size = 4 + byte_size(Packet),",
            "case Size rem 4 of", "0 -> {ok, Size, <<>>};",
            "2 -> {ok, Size + 2, << 0:16 >>};", "_ -> {error, badarg}", "end.")));

    assertThat(p).matches(
        (code("-module(m).", "hexstring(<< X:128/big-unsigned-integer >>) -> ",
            "lists:flatten(io_lib:format(\"~32.16.0b\", [X])).")));

    assertThat(p)
        .matches(
            (code("-module(m).", "sys_info() ->",
                "SysArch = string:strip(erlang:system_info(system_architecture),right,$\\n).")));
  }

  @Test
  public void funWithArity() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "dodo(A) ->",
                "Properties = lists:map(fun list_to_atom/1, string:tokens(proplists:get_value(properties, Opts,\"\"),\",\")).")));
  }

  @Test
  public void emptyArgFuncCall() throws IOException, URISyntaxException {

    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "integer().")));
  }

  @Test
  public void recordSet() throws IOException, URISyntaxException {
    assertThat(p).matches((code("-module(m).", "dodo(A) ->", "#msg{to=void, no=3}.")));
  }

  @Test
  public void ifTest() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "dodo(A) ->",
                "if A =:= B -> ok; true -> io:format(\"assert error in module ~p on line ~p~n\", [?MODULE, ?LINE]) end.")));
  }

  @Test
  public void recordInFuncMatch() {
    assertThat(p).matches(
        (code("-module(m).", "send_010a(ItemsList, Client=#client{gid=DestGID}) ->",
            "true.")));

  }

  @Test
  public void recordInFuncCall() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "dodo(A) ->",
                "case mnesia:read(user, Username) of",
                "[User] -> mnesia:write(User#user{ibuttons = User#user.ibuttons ++ [IButton]});",
                "E -> E", "end.")));
  }

  @Test
  public void recordSetWithListExp() {
    assertThat(p).matches(
        (code("-module(m).", "dodo(A) ->",
            "User#user{ibuttons = User#user.ibuttons ++ [IButton]}", ".")));
  }

  @Test
  public void nestedBinaryMatch() {
    assertThat(p).matches(
        (code("-module(m).", "dodo(A) ->",
            "UCS2Name = << << X:8, 0:8 >> || << X >> <= <<Name>> >>.")));
  }

  @Test
  public void exports() throws IOException, URISyntaxException {

    assertThat(p).matches(
        (code("-module(m).", "-export([dodo/1]).", "-export(dodo/2).", "-export([]).",
            "dodo(A) ->", "{a, node()}.")));
  }

  @Test
  public void typeTest() throws IOException, URISyntaxException {

    assertThat(p).matches(
        (code("-module(m).", "-export(dodo/1).", "-type my_type() :: atom() | integer().",
            "dodo(A) ->", "{a, node()}.")));

    assertThat(p)
        .matches(
            (code("-module(m).",
                "-export(dodo/1).",
                "-type my_type() :: {non_reg_integer(), non_reg_integer(), non_reg_integer()}.",
                "dodo(A) ->", "{a, node()}.")));

    assertThat(p).matches(
        (code("-module(m).", "-export(dodo/1).", "-type(gid_record() :: #gid_record{}).",
            "dodo(A) ->", "{a, node()}.")));
  }

  @Test
  public void macroDefine() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "-define(ASSERT_EQ(A, B), if A =:= B -> ok; true -> io:format(\"assert error in module ~p on line ~p~n\", [?MODULE, ?LINE]) end).",
                "dodo(A) ->", "{a, node()}.")));
  }

  @Test
  public void flowControlMacros() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "-spec dodo(integer()) -> atom().",
                "-ifdef(debug).",
                "-define(LOG(X), io:format(\"{~p,~p}: ~p~n\", [?MODULE,?LINE,X])).",
                "-else.", "-define(LOG(X), true).", "-endif.", "dodo(A) ->",
                "{a, node()}.")));

    assertThat(p)
        .matches(
            (code("-module(m).", "-ifdef(debug).", "dodo(A) ->", "{a, node()}.",
                "-else.", "dodo(A) ->", "{a, node2()}.", "-endif.",
                "dodo(A, B) ->", "{a, node()}.")));

  }

  @Test
  public void specTest() {
    assertThat(p).matches(
        (code("-module(m).",
            "-spec split_nodename(atom() | string()) -> {atom(), nonempty_string()}.",
            "dodo(A) ->", "{a, node()}.")));

  }

  @Test
  public void moduleAttrTest() {
    assertThat(p).matches(
        (code("-module(m).", "-ignore_xref([{json, decode, 1}]).", "dodo(A) ->",
            "{a, node()}.")));
  }

  @Test
  public void moduleEverythingInIfTest() {
    assertThat(p).matches(
        (code("-module(m).", "-ifdef(A).", "dodo(A) ->", "{a, node()}.", "-else.",
            "dodo(A) ->", "{a, node()}.", "-endif.")));
  }

  @Test
  public void linebreakInMethodCall() {
    assertThat(p).matches(
        (code("-module(m).", "ordinal(N) when is_integer(N) -> io_lib:format(\"~wth\"",
            "                                                        ,[N]).")));
  }

  @Test
  public void linebreakInStringLiteral() {
    assertThat(p)
        .matches(
            (code("-module(m).",
                "ordinal(N) when is_integer(N) -> io_lib:format(\"This is a multiline string that spans two lines\\n\"\n\"using two source lines for convenience\").")));

    assertThat(p)
        .matches(
            (code("-module(m).",
                "ordinal(N) when is_integer(N) -> io_lib:format(\"This is a multiline string that spans two lines\\n\"\n\r\t \t\"using two source lines for convenience\").")));

  }

  @Test
  public void macroOutsideOfFunction() {
    assertThat(p).matches(
        (code("-module(m).", "-define(TIME(A),-decorate({})).", "?TIME(huh).",
            "a() -> error.")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

  @After
  public void log() {
    ExtendedStackTraceStream.print(listener, System.out);
  }
}
