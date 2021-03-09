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
import org.sonar.sslr.grammar.LexerlessGrammarBuilder;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserBinaryExpressionTest {

  private LexerlessGrammarBuilder b = ErlangGrammarImpl.createGrammarBuilder();

  @Test
  public void binaryExpressions() {
    assertThat(b.build().rule(ErlangGrammarImpl.binaryLiteral))
      .matches("<<1,17,42>>")
      .matches("<<1,17,42:16>>")
      .matches("<<1024/utf8>>")
      .matches("<<1024:16/utf8>>")
      .matches("<<$a,$b,$c>>")
      .matches("<<\"hello\">>")
      .matches("<<A,B,C:16>>")
      .matches("<<G,H/binary>>")
      .matches("<<G,H:16/bitstring>>")
      .matches("<< << X:8, 0:8/utf8 >> || << X >> <= << 1, A, 3 >> >>")
      .matches("<<\n?MAGIC,\nVersion:?BYTE,\nType:?BYTE,\n>>")
      .matches("<< << (X*2) >> || <<X>> <= << 1,2,3 >> >>")
      .matches("[ << (X*2) >> || <<X>> <= << 1,2,3 >> ]")
      .matches("<< << (X*2) >> || <<X>> <= method1() >>")
      .matches("<< << (X*2) >> || <<X>> <= method1(), method2() >>")
      .matches("[ << (X*2) >> || <<X>> <= method1(), method2() ]")
      .matches("<<Part1:4/big-unsigned-integer-unit:8," +
        "Part2:4/big-unsigned-integer-unit:8," +
        "Body/binary>>")
      .matches("<<A:(12 + 4)>>");
  }
}
