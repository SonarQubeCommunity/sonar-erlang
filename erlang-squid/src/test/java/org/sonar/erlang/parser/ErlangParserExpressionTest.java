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

import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserExpressionTest {

  private LexerlessGrammar g = ErlangGrammarImpl.createGrammar();

  @Test
  public void simpleExpression() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("1+3")
        .matches("true")
        .matches("6 + 5 * 4 - 3 / 2")
        .matches("ok");
  }

  @Test
  public void simpleExpressionWithP() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("(1+3)")
        .matches("(6 + 5) * ((4 - 3) / 2)");

  }

  @Test
  public void varMatch() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("A=2")
        .matches("A=-2")
        .matches("A=N-2")
        .matches("A=N--2")
        .matches("B=[2,3]")
        .matches("B={2,3}");

  }

  @Test
  public void relationExpression() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("A+5>=12");

  }

  @Test
  public void listExpression() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("[asd,ore,[ow,2,3],[hello,23]]")
        .matches("[]")
        .matches("[d|T]")
        .matches("[c|[]]")
        .matches("[a|[b|[c|[]]]]")
        .matches("[a,2,{c,4}]")
        .matches(
            "[Name,proplists:get_value(description,Spec,[])|proplists:get_value(keywords,Spec,[])]");

  }

  @Test
  public void tupleExpression() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("{asd,ore,{ow,[2,23,as],3},[hello,{23,as}]}")
        .matches("{float(), float()}")
        .matches("{Rest2, catch list_to_integer(Digits)}");

  }

  @Test
  public void binaryExpression() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("<<A,B,C:16>> = <<1,17,42:16>>")
        .matches("<<D:16,E,F>> = <<1,17,42:16>>")
        .matches("<<G,H/binary>> = <<1,17,42:16>>")
        .matches("<<G,H/bitstring>> = <<1,17,42:12>>");
  }

  @Test
  public void listComprehensionExpressin() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("[X*2 || X <- [1,2,3]]")
        .matches("[X*2 || X <- method()]")
        .matches("[X*2 || X <- method(), method2()]")
        .matches("[X*2 || X <- [1,2,3]] ++ [7,8,9]")
        .matches("[X*2 || X <- [1,2,3]] -- [7,8,9]")
        .matches("[10, 23] -- [X*2 || X <- [1,2,3]] ++ [7,8,9]")
        .matches("[756, 877] ++ [X*2 || X <- [1,2,3]] -- [7,8,9]")
        .matches("[{A,B} || {A, B} <- method(), method2(File)]")
        .matches("[Call || {_From, To} = Call <- ExtCalls, lists:member(To, RelevantAPICalls)]")
        .matches("[{M, F, A} || {nowarn_unused_function, FAs} <- Opts,  {F, A} <- lists:flatten([FAs])]")
        .matches("[Call || Call = {_From, To} <- ExtCalls, not dialyzer_plt:contains_mfa(InitPlt, To)]");
  }

  @Test
  public void logicalExpressions() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("not true")
        .matches("true and false")
        .matches("true xor false")
        .matches("true or A")
        .matches("A orelse B")
        .matches("A andalso B")
        .matches("not A andalso B or false")
        .matches("(not (A andalso B)) or false");
  }

  @Test
  public void catchExpressions() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("catch 1+2")
        .matches("catch 1+a")
        .matches("A = (catch 1+2)")
        .matches("catch throw(hello)");
  }

  @Test
  public void recordCreate() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("#Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")
        .matches("#person{name=Name, _='_'}")
        .matches("A = #Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")
        .matches("S = #person{name=Name, _='_'}")
        .matches("User#user{ibuttons = User#user.ibuttons ++ [IButton]}");
  }

  @Test
  public void recordAccess() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("#person.name")
        .matches("Expr#Name.Field")
        .matches("N2#nrec2.nrec1#nrec1.nrec0.nrec00#nrec0.name.first")
        .matches("N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = \"nested0a\"}")
        .matches("(PartialMsg#'MegacoMessage'.mess)#'Message'.version");

  }

  @Test
  public void macroUse() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("?TIMEOUT")
        .matches("?MACRO1(a, b)")
        .matches("?MACRO1(X, 123)")
        .matches("server:call(refserver, Request, ?TIMEOUT)")
        .matches("server:call(refserver, Request, ?MACRO1(a, b))")
        .matches("?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents)");
  }

  @Test
  public void minus() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("t_from_range(-(1 bsl (N - 1)), 1 bsl (N - 1) - 1)");
  }

  @Test
  public void string() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
        .matches("\"asdasd\"")
        .matches("\"asdasd\" \"asdasd\"")
        .matches("\"asdasd\" \"asdasd\"\n \"effef\"")
        .matches("\"asdasd\" ?MARCI\n \"effef\"");
  }

}
