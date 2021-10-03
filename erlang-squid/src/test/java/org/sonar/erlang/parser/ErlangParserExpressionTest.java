/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 * Copyright © 2021 Daniils Petrovs <dpetrovs@evolution.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.sonar.erlang.parser;

import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserExpressionTest {

  private final LexerlessGrammar g = ErlangGrammarImpl.createGrammar();

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
    assertThat(g.rule(ErlangGrammarImpl.assignmentExpression))
      .matches("A=2")
      .matches("A = 3")
      .matches("A=-2")
      .matches("A=N-2")
      .matches("A=N--2")
      .matches("B=[2,3]")
      .matches("B={2,3}")
      .matches("B=A")
      .matches("B=A=error");

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
      .matches("{Rest2, catch list_to_integer(Digits)}")
      .matches("{ query, Query}");

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
      .matches("[Call || Call = {_From, To} <- ExtCalls, not dialyzer_plt:contains_mfa(InitPlt, To)]")
      .matches("[ Pid ! Msg || Pid <- ListOfPids ]");
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
      .matches("(not (A andalso B)) or false")
      .matches("bnot A")
      .matches("Z band (bnot D)");
  }

  @Test
  public void catchExpressions() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
      .matches("catch 1+2")
      .matches("catch 1+a")
      .matches("A = (catch 1+2)")
      .matches("catch throw(hello)")
      .matches("catch encode({x,y})");
  }

  @Test
  public void recordCreate() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
      .matches("#Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")
      .matches("#person{name=Name, _='_'}")
      .matches("?Macro{name=Name}")
      .matches("A = #Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")
      .matches("S = #person{name=Name, _='_'}")
      .matches("User#user{ibuttons = User#user.ibuttons ++ [IButton]}")
      .matches("#input{field=#person{name=Name}=Boss, partner=Partner}")
      .matches("N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = \"nested0a\"}")
      .matches("Rec?FOO_REC{bar = \"hello\"}");
  }

  @Test
  public void recordAccess() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
      // .matches("#person.name")
      .notMatches("Rec#a.key.\nasd")
      .matches("Expr#person.name")
      .matches("N2#nrec2.nrec1#nrec1.nrec0.nrec00#nrec0.name.first")
      .matches("(PartialMsg#'MegacoMessage'.mess)")
      .matches("(PartialMsg#'MegacoMessage'.mess)#'Message'.version")
      .matches("Record#my_record.key1")
      .matches("Rec?FOO_REC.bar");

  }

  @Test
  public void macroUse() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
      .matches("?TIMEOUT")
      .matches("?MACRO1(a, b)")
      .matches("?MACRO1(X, 123)")
      .matches("server:call(refserver, Request, ?TIMEOUT)")
      .matches("server:call(refserver, Request, ?MACRO1(a, b))")
      .matches("?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents)")
      .matches("?assertEqual({'EXIT',{json_encode,{bad_term,{x,y}}}}, catch encode({x,y}))")
      .matches("??A");
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

  @Test
  public void mapCreate() {
    assertThat(g.rule(ErlangGrammarImpl.mapCreateUpdate))
      .matches("a => 1")
      .matches("a => <<\"afsfas\">>");
    assertThat(g.rule(ErlangGrammarImpl.mapCreateUpdate))
      .matches("a := 1")
      .matches("a := <<\"afsfas\">>");
    assertThat(g.rule(ErlangGrammarImpl.map))
      .matches("#{}")
      .matches("#{a => <<\"hello\">>}")
      .matches("#{1 => 2, b => b}")
      .matches("#{k => {A,B}}")
      .matches("#{k => {A,B}}")
      .matches("#{{\"w\", 1} => f()}")
      .matches("#{1 => a, 1 => b}")
      .matches("#{1.0 => a, 1 => b}");
    assertThat(g.rule(ErlangGrammarImpl.assignmentExpression))
      .matches("M0 = #{1 => 2}")
      .matches("M0 = #{}");
    assertThat(g.rule(ErlangGrammarImpl.statements))
      .matches("M0 = #{}")
      .matches("M0 = #{},                 % empty map\n" +
        "M1 = #{a => <<\"hello\">>}, % single association with literals\n" +
        "M2 = #{1 => 2, b => b},   % multiple associations with literals\n" +
        "M3 = #{k => {A,B}},       % single association with variables\n" +
        "M4 = #{{\"w\", 1} => f()}  % compound key associated with an evaluated expression")
      .matches("#{ \"vendorservice\" := VendorService, \"apis\" := Apis } = Service");
  }

  @Test
  public void mapUpdate() {
    assertThat(g.rule(ErlangGrammarImpl.statements))
      .matches("M0#{a => 0}")
      .matches("M1#{a => 1, b => 2}")
      .matches("M2#{\"function\" => fun() -> f() end}")
      .matches("M3#{a := 2, b := 3}")
      .matches("M0 = #{},\n" +
              "M1 = M0#{a => 0},\n" +
              "M2 = M1#{a => 1, b => 2},\n" +
              "M3 = M2#{\"function\" => fun() -> f() end},\n" +
              "M4 = M3#{a := 2, b := 3}  % 'a' and 'b' was added in `M1` and `M2`.");
  }

  @Test
  public void mapInPattern() {
    assertThat(g.rule(ErlangGrammarImpl.expression))
            .matches("#{\"tuple\" := {1,B}} = M");

  }

}
