/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2018 Tamas Kende; Denes Hegedus (Cursor Insight Ltd.)
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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.erlang.parser;

import org.junit.Before;
import org.junit.Test;
import org.sonar.sslr.grammar.LexerlessGrammarBuilder;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

//TODO: why do I have to add a whitespace before+after: ,; when I mock?
public class ErlangParserIfExpressionTest {
  private LexerlessGrammarBuilder g;

  @Before
  public void setUp() {
    g = ErlangGrammarImpl.createGrammarBuilder();
  }

  @Test
  public void ifSimple() {
    g.rule(ErlangGrammarImpl.branchExps).override("branchExps ");
    assertThat(g.build().rule(ErlangGrammarImpl.memberExpression))
      .matches("if branchExps end");
  }

  @Test
  public void ifSimple2() {
    g.rule(ErlangGrammarImpl.branchExp).override("branchExp ");
    assertThat(g.build().rule(ErlangGrammarImpl.memberExpression))
      .matches("if branchExp ; branchExp end");
  }

  @Test
  public void ifSimple3() {
    g.rule(ErlangGrammarImpl.guardSequence).override("guardSequence ");
    g.rule(ErlangGrammarImpl.assignmentExpression).override("assignmentExpression ");
    assertThat(g.build().rule(ErlangGrammarImpl.memberExpression))
      .matches(
        "if guardSequence -> assignmentExpression , assignmentExpression end")
      .matches(
        "if guardSequence -> assignmentExpression , assignmentExpression ; guardSequence -> assignmentExpression end");
  }

  @Test
  public void ifSimple4() {
    g.rule(ErlangGrammarImpl.guard).override("guard ");
    g.rule(ErlangGrammarImpl.assignmentExpression).override("assignmentExpression ");
    assertThat(g.build().rule(ErlangGrammarImpl.memberExpression))
      .matches(
        "if guard ; guard ; guard -> assignmentExpression , assignmentExpression end")
      .matches(
        "if guard ; guard -> assignmentExpression , assignmentExpression ; guard ; guard -> assignmentExpression end");
  }

  @Test
  public void ifSimple5() {
    g.rule(ErlangGrammarImpl.guardExpression).override("guardExpression ");
    g.rule(ErlangGrammarImpl.assignmentExpression).override("assignmentExpression ");
    assertThat(g.build().rule(ErlangGrammarImpl.memberExpression))
      .matches(
        "if guardExpression , guardExpression ; guardExpression ; guardExpression , guardExpression , guardExpression -> assignmentExpression , assignmentExpression end")
      .matches(
        "if guardExpression ; guardExpression , guardExpression -> assignmentExpression , assignmentExpression ; guardExpression , guardExpression ; guardExpression -> assignmentExpression end");
  }

}
