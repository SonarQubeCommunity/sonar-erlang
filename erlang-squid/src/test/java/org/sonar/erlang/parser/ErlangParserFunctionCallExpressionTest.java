/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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

import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserFunctionCallExpressionTest {
  private LexerlessGrammar g = ErlangGrammarImpl.createGrammar();

  @Test
  public void functionCallExpressions() {
    assertThat(g.rule(ErlangGrammarImpl.statement))
      .matches("method(\"hello\")")
      .matches("method(12)")
      .matches("method(\"hello\",234234)")
      .matches("haho:method(\"hello\")")
      .matches("method(\"hello\")")
      .matches("io:format(\"assert error in module ~p on line ~p~n\")")
      .matches(
        "string:strip(erlang:system_info(system_architecture),right,$\n)")
      .matches("lists:reverse ([$\\] | L])")
      .matches("?assertMatch([{ok, 1, Pid}| _] when is_pid(Pid), Result)")
      .matches("format_querystring([{outputtype, json}\u00A0|\u2003Params])");
  }

}
