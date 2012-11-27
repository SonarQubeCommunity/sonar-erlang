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

public class ErlangParserCaseStatementTest {
  ExtendedStackTrace listener = new ExtendedStackTrace();
  Parser<ErlangGrammar> p = ErlangParser
      .create(new ErlangConfiguration(Charsets.UTF_8), listener);

  ErlangGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.caseExpression);
  }

  @Test
  public void caseSimple1() {
    g.assignmentExpression.mock();
    g.patternStatements.mock();
    assertThat(p).matches((code("case assignmentExpression of patternStatements end")));
  }

  @Test
  public void caseSimple2() {
    g.assignmentExpression.mock();
    g.patternStatement.mock();
    assertThat(p).matches((code("case assignmentExpression of patternStatement end")));
    assertThat(p).matches(
        (code("case assignmentExpression of patternStatement; patternStatement end")));
  }

  @Test
  public void caseSimple3() {
    g.assignmentExpression.mock();
    g.patternStatement.mock();
    assertThat(p).matches((code("case assignmentExpression of patternStatement end")));
    assertThat(p).matches(
        (code("case assignmentExpression of patternStatement; patternStatement end")));
  }

  @Test
  public void caseReal1() {
    assertThat(p).matches(
        (code("case cerl:is_c_var(PosVar) andalso (cerl:var_name(PosVar) =/= '') of",
            "true -> \"variable \"++String;", "false -> \"pattern \"++String", "end")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

  @After
  public void log() {
    ExtendedStackTraceStream.print(listener, System.out);
  }

}
