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

public class ErlangParserBinaryExpressionTest {
  ExtendedStackTrace listener = new ExtendedStackTrace();
  Parser<ErlangGrammar> p = ErlangParser
      .create(new ErlangConfiguration(Charsets.UTF_8), listener);

  ErlangGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.binaryLiteral);
  }

  @Test
  public void binaryExpressions() {
    assertThat(p).matches(code("<<1,17,42>>"));
    assertThat(p).matches(code("<<1,17,42:16>>"));
    assertThat(p).matches(code("<<1024/utf8>>"));
    assertThat(p).matches(code("<<1024:16/utf8>>"));
    assertThat(p).matches(code("<<$a,$b,$c>>"));
    assertThat(p).matches(code("<<\"hello\">>"));
    assertThat(p).matches(code("<<A,B,C:16>>"));
    assertThat(p).matches(code("<<G,H/binary>>"));
    assertThat(p).matches(code("<<G,H:16/bitstring>>"));
    assertThat(p).matches(code("<< << X:8, 0:8/utf8 >> || << X >> <= << 1, A, 3 >> >>"));
    assertThat(p).matches(code("<<", "?MAGIC,", "Version:?BYTE,", "Type:?BYTE,", ">>"));
    assertThat(p).matches((code("<< << (X*2) >> || <<X>> <= << 1,2,3 >> >>")));
    assertThat(p).matches((code("<< << (X*2) >> || <<X>> <= method1() >>")));
    assertThat(p).matches((code("<< << (X*2) >> || <<X>> <= method1(), method2() >>")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

  @After
  public void log() {
    ExtendedStackTraceStream.print(listener, System.out);
  }

}
