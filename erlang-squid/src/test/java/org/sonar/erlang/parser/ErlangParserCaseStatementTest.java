/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
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

import com.google.common.base.Joiner;
import org.junit.Before;
import org.junit.Test;
import org.sonar.sslr.grammar.LexerlessGrammarBuilder;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserCaseStatementTest {

  private LexerlessGrammarBuilder grammar;

  @Before
  public void setUp() {
    grammar = ErlangGrammarImpl.createGrammarBuilder();
  }

  @Test
  public void caseSimple1() {
    // caseKeyword, expression, ofKeyword, patternStatements, endKeyword
    grammar.rule(ErlangGrammarImpl.expression).override("expression ");
    grammar.rule(ErlangGrammarImpl.patternStatements).override("patternStatements ");
    assertThat(grammar.build().rule(ErlangGrammarImpl.caseExpression))
      .matches(code("case expression of patternStatements end"));
  }

  @Test
  public void caseSimple2() {
    grammar.rule(ErlangGrammarImpl.expression).override("expression ");
    grammar.rule(ErlangGrammarImpl.patternStatement).override("patternStatement ");
    assertThat(grammar.build().rule(ErlangGrammarImpl.caseExpression))
      .matches("case expression of patternStatement end")
      .matches("case expression of patternStatement ; patternStatement end");
  }

  @Test
  public void caseReal1() {
    assertThat(grammar.build().rule(ErlangGrammarImpl.caseExpression)).matches(
      (code("case cerl:is_c_var(PosVar) andalso (cerl:var_name(PosVar) =/= '') of",
        "true -> \"variable \"++String;", "false -> \"pattern \"++String", "end")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

}
