/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2016 Tamas Kende
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

import com.google.common.base.Joiner;
import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserCaseStatementTest {

  private LexerlessGrammar g = ErlangGrammarImpl.createGrammar();

  @Test
  public void caseSimple1() {
    g.rule(ErlangGrammarImpl.expression).mock();
    g.rule(ErlangGrammarImpl.patternStatements).mock();
    assertThat(g.rule(ErlangGrammarImpl.caseExpression))
      .matches((code("case expression of patternStatements end")));
  }

  @Test
  public void caseSimple2() {
    g.rule(ErlangGrammarImpl.expression).mock();
    g.rule(ErlangGrammarImpl.patternStatement).mock();
    assertThat(g.rule(ErlangGrammarImpl.caseExpression))
      .matches("case expression of patternStatement end")
      .matches("case expression of patternStatement ; patternStatement end");
  }

  @Test
  public void caseReal1() {
    assertThat(g.rule(ErlangGrammarImpl.caseExpression)).matches(
      (code("case cerl:is_c_var(PosVar) andalso (cerl:var_name(PosVar) =/= '') of",
        "true -> \"variable \"++String;", "false -> \"pattern \"++String", "end")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

}
