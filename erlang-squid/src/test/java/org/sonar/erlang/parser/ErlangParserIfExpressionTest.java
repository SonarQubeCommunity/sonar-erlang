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

import com.google.common.base.Joiner;
import com.sonar.sslr.api.Rule;
import org.junit.Before;
import org.junit.Test;
import org.sonar.erlang.api.ErlangGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

//TODO: why do I have to add a whitespace before+after: ,; when I mock?
public class ErlangParserIfExpressionTest {
  ErlangGrammar g = new ErlangGrammarImpl();
  Rule p = g.memberExpression;

  @Before
  public void setup() {
    g = new ErlangGrammarImpl();
    p = g.memberExpression;
  }

  @Test
  public void ifSimple() {
    g.branchExps.mock();
    assertThat(p).matches((code("if branchExps end")));
  }

  @Test
  public void ifSimple2() {
    g.branchExp.mock();
    assertThat(p).matches((code("if branchExp ; branchExp end")));
  }

  @Test
  public void ifSimple3() {
    g.guardSequence.mock();
    g.assignmentExpression.mock();
    assertThat(p).matches(
        code("if guardSequence -> assignmentExpression , assignmentExpression end"));
    assertThat(p)
        .matches(
            code("if guardSequence -> assignmentExpression , assignmentExpression ; guardSequence -> assignmentExpression end"));
  }

  @Test
  public void ifSimple4() {
    g.guard.mock();
    g.assignmentExpression.mock();
    assertThat(p).matches(
        code("if guard ; guard ; guard -> assignmentExpression , assignmentExpression end"));
    assertThat(p)
        .matches(
            code("if guard ; guard -> assignmentExpression , assignmentExpression ; guard ; guard -> assignmentExpression end"));
  }

  @Test
  public void ifSimple5() {
    g.guardExpression.mock();
    g.assignmentExpression.mock();
    assertThat(p)
        .matches(
            (code("if guardExpression , guardExpression ; guardExpression ; guardExpression , guardExpression , guardExpression -> assignmentExpression , assignmentExpression end")));
    assertThat(p)
        .matches(
            (code("if guardExpression ; guardExpression , guardExpression -> assignmentExpression , assignmentExpression ; guardExpression , guardExpression ; guardExpression -> assignmentExpression end")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

}
