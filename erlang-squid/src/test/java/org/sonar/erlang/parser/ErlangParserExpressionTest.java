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

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserExpressionTest {
  ExtendedStackTrace listener = new ExtendedStackTrace();
  Parser<ErlangGrammar> p = ErlangParser
      .create(new ErlangConfiguration(Charsets.UTF_8), listener);

  ErlangGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.expression);
  }

  @Test
  public void simpleExpression() {
    assertThat(p).matches((code("1+3")));
    assertThat(p).matches((code("true")));
    assertThat(p).matches((code("6 + 5 * 4 - 3 / 2")));
    assertThat(p).matches((code("ok")));
  }

  @Test
  public void simpleExpressionWithP() {
    assertThat(p).matches((code("(1+3)")));
    assertThat(p).matches(("(6 + 5) * ((4 - 3) / 2)"));

  }

  @Test
  public void varMatch() {
    assertThat(p).matches((code("A=2")));
    assertThat(p).matches((code("A=-2")));
    assertThat(p).matches((code("A=N-2")));
    assertThat(p).matches((code("A=N--2")));
    assertThat(p).matches((code("B=[2,3]")));
    assertThat(p).matches((code("B={2,3}")));

  }

  @Test
  public void relationExpression() {
    assertThat(p).matches((code("A+5>=12")));

  }

  @Test
  public void listExpression() {
    assertThat(p).matches((code("[asd,ore,[ow,2,3],[hello,23]]")));
    assertThat(p).matches((code("[]")));
    assertThat(p).matches((code("[d|T]")));
    assertThat(p).matches((code("[c|[]]")));
    assertThat(p).matches((code("[a|[b|[c|[]]]]")));
    assertThat(p).matches((code("[a,2,{c,4}]")));
    assertThat(p)
        .matches(
            (code("[Name,proplists:get_value(description,Spec,[])|proplists:get_value(keywords,Spec,[])]")));

  }

  @Test
  public void tupleExpression() {
    assertThat(p).matches((code("{asd,ore,{ow,[2,23,as],3},[hello,{23,as}]}")));
    assertThat(p).matches((code("{float(), float()}")));
    assertThat(p).matches((code("{Rest2, catch list_to_integer(Digits)}")));

  }

  @Test
  public void binaryExpression() {
    assertThat(p).matches((code("<<A,B,C:16>> = <<1,17,42:16>>")));
    assertThat(p).matches((code("<<D:16,E,F>> = <<1,17,42:16>>")));
    assertThat(p).matches((code("<<G,H/binary>> = <<1,17,42:16>>")));
    assertThat(p).matches((code("<<G,H/bitstring>> = <<1,17,42:12>>")));
  }

  @Test
  public void listComprehensionExpressin() {
    assertThat(p).matches((code("[X*2 || X <- [1,2,3]]")));
    assertThat(p).matches((code("[X*2 || X <- method()]")));
    assertThat(p).matches((code("[X*2 || X <- method(), method2()]")));
    assertThat(p).matches((code("[X*2 || X <- [1,2,3]] ++ [7,8,9]")));
    assertThat(p).matches((code("[X*2 || X <- [1,2,3]] -- [7,8,9]")));
    assertThat(p).matches((code("[10, 23] -- [X*2 || X <- [1,2,3]] ++ [7,8,9]")));
    assertThat(p).matches((code("[756, 877] ++ [X*2 || X <- [1,2,3]] -- [7,8,9]")));
    assertThat(p).matches((code("[{A,B} || {A, B} <- method(), method2(File)]")));
    assertThat(p)
        .matches(
            (code("[Call || {_From, To} = Call <- ExtCalls, lists:member(To, RelevantAPICalls)]")));
    assertThat(p)
        .matches(
            (code("[{M, F, A} || {nowarn_unused_function, FAs} <- Opts,  {F, A} <- lists:flatten([FAs])]")));
    assertThat(p)
        .matches(
            (code("[Call || Call = {_From, To} <- ExtCalls, not dialyzer_plt:contains_mfa(InitPlt, To)]")));
  }

  @Test
  public void logicalExpressions() {
    assertThat(p).matches((code("not true")));
    assertThat(p).matches((code("true and false")));
    assertThat(p).matches((code("true xor false")));
    assertThat(p).matches((code("true or A")));
    assertThat(p).matches((code("A orelse B")));
    assertThat(p).matches((code("A andalso B")));
    assertThat(p).matches((code("not A andalso B or false")));
    assertThat(p).matches((code("(not (A andalso B)) or false")));
  }

  @Test
  public void catchExpressions() {
    assertThat(p).matches((code("catch 1+2")));
    assertThat(p).matches((code("catch 1+a")));
    assertThat(p).matches((code("A = (catch 1+2)")));
    assertThat(p).matches((code("catch throw(hello)")));
  }

  @Test
  public void recordCreate() {
    p.setRootRule(g.expression);
    assertThat(p).matches((code("#Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")));
    assertThat(p).matches((code("#person{name=Name, _='_'}")));
    assertThat(p).matches((code("A = #Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")));
    assertThat(p).matches((code("S = #person{name=Name, _='_'}")));
    assertThat(p).matches((code("User#user{ibuttons = User#user.ibuttons ++ [IButton]}")));
  }

  @Test
  public void recordAccess() {
    assertThat(p).matches((code("#person.name")));
    assertThat(p).matches((code("Expr#Name.Field")));
    assertThat(p).matches((code("N2#nrec2.nrec1#nrec1.nrec0.nrec00#nrec0.name.first")));
    assertThat(p).matches((code("N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = \"nested0a\"}")));
    assertThat(p).matches((code("(PartialMsg#'MegacoMessage'.mess)#'Message'.version")));

  }

  @Test
  public void macroUse() {
    assertThat(p).matches((code("?TIMEOUT")));
    assertThat(p).matches((code("?MACRO1(a, b)")));
    assertThat(p).matches((code("?MACRO1(X, 123)")));
    assertThat(p).matches((code("server:call(refserver, Request, ?TIMEOUT)")));
    assertThat(p).matches((code("server:call(refserver, Request, ?MACRO1(a, b))")));
    assertThat(p).matches(
        (code("?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents)")));
  }

  @Test
  public void minus() {
    assertThat(p).matches((code("t_from_range(-(1 bsl (N - 1)), 1 bsl (N - 1) - 1)")));
  }

  @Test
  public void string() {
    assertThat(p).matches((code("\"asdasd\"")));
    assertThat(p).matches((code("\"asdasd\" \"asdasd\"")));
    assertThat(p).matches((code("\"asdasd\" \"asdasd\"\n \"effef\"")));
    assertThat(p).matches((code("\"asdasd\" ?MARCI\n \"effef\"")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

  @After
  public void log() {
    try {
      ExtendedStackTraceStream.print(listener, System.out);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
