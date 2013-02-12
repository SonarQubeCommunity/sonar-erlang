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

//TODO: why do I have to add a whitespace before+after: ,; when I mock?
public class ErlangParserIfExpressionTest {
  private LexerlessGrammar g = ErlangGrammarImpl.createGrammar();

  @Test
  public void ifSimple() {
    g.rule(ErlangGrammarImpl.branchExps).mock();
    assertThat(g.rule(ErlangGrammarImpl.memberExpression))
        .matches("if branchExps end");
  }

  @Test
  public void ifSimple2() {
    g.rule(ErlangGrammarImpl.branchExp).mock();
    assertThat(g.rule(ErlangGrammarImpl.memberExpression))
        .matches("if branchExp ; branchExp end");
  }

  @Test
  public void ifSimple3() {
    g.rule(ErlangGrammarImpl.guardSequence).mock();
    g.rule(ErlangGrammarImpl.assignmentExpression).mock();
    assertThat(g.rule(ErlangGrammarImpl.memberExpression))
        .matches(
            "if guardSequence -> assignmentExpression , assignmentExpression end")
        .matches(
            "if guardSequence -> assignmentExpression , assignmentExpression ; guardSequence -> assignmentExpression end");
  }

  @Test
  public void ifSimple4() {
    g.rule(ErlangGrammarImpl.guard).mock();
    g.rule(ErlangGrammarImpl.assignmentExpression).mock();
    assertThat(g.rule(ErlangGrammarImpl.memberExpression))
        .matches(
            "if guard ; guard ; guard -> assignmentExpression , assignmentExpression end")
        .matches(
            "if guard ; guard -> assignmentExpression , assignmentExpression ; guard ; guard -> assignmentExpression end");
  }

  @Test
  public void ifSimple5() {
    g.rule(ErlangGrammarImpl.guardExpression).mock();
    g.rule(ErlangGrammarImpl.assignmentExpression).mock();
    assertThat(g.rule(ErlangGrammarImpl.memberExpression))
        .matches(
            "if guardExpression , guardExpression ; guardExpression ; guardExpression , guardExpression , guardExpression -> assignmentExpression , assignmentExpression end")
        .matches(
            "if guardExpression ; guardExpression , guardExpression -> assignmentExpression , assignmentExpression ; guardExpression , guardExpression ; guardExpression -> assignmentExpression end");
  }

}
