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

import org.sonar.sslr.grammar.LexerlessGrammarBuilder;

import com.sonar.sslr.api.Rule;
import org.junit.Test;
import org.sonar.erlang.api.ErlangGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserLowLevelTest {

  ErlangGrammar g = new ErlangGrammarImpl();

  private LexerlessGrammarBuilder b = ErlangGrammarImpl2.createGrammarBuilder();

  @Test
  public void lexInlineComment() {
    Rule p = g.spacing;
    assertThat(p).matches("% My Comment new line");
    assertThat(p).matches("%");
    assertThat(p).matches("%%%");
    assertThat(p).matches("%%%------------------------------\n%% asdasd");
  }

  @Test
  public void numericLiteral() {
    assertThat(b.build().rule(ErlangGrammarImpl2.numericLiteral))
    .matches("0")
    .matches("0")
    .matches("123")

    .matches("123.456")

    .matches("123.456e10")
    .matches("123.456e-10")

    .matches("123.456E10")
    .matches("123.456E-10")

    .matches("0.123")

    .matches("0.123e4")
    .matches("0.123e-4")

    .matches("0.123E4")
    .matches("0.123E-4")

    .matches("2#12")
    .matches("16#1f")
    .matches("$\\n")
    .matches("$\\")
    .matches("$w")
    .matches("$\\b")
    .matches("$\\123")
    .matches("$\\xA0")
    .matches("$\\x{A2F}")
    .matches("$\\^A")
    .matches("$\\\"")
    .matches("$\\\\");

    Rule p = g.numericLiteral;

    assertThat(p).matches("0");
    assertThat(p).matches("123");

    assertThat(p).matches("123.456");

    assertThat(p).matches("123.456e10");
    assertThat(p).matches("123.456e-10");

    assertThat(p).matches("123.456E10");
    assertThat(p).matches("123.456E-10");

    assertThat(p).matches("0.123");

    assertThat(p).matches("0.123e4");
    assertThat(p).matches("0.123e-4");

    assertThat(p).matches("0.123E4");
    assertThat(p).matches("0.123E-4");

    assertThat(p).matches("2#12");
    assertThat(p).matches("16#1f");
    assertThat(p).matches("$\\n");
    assertThat(p).matches("$\\");
    assertThat(p).matches("$w");
    assertThat(p).matches("$\\b");
    assertThat(p).matches("$\\123");
    assertThat(p).matches("$\\xA0");
    assertThat(p).matches("$\\x{A2F}");
    assertThat(p).matches("$\\^A");
    assertThat(p).matches("$\\\"");
    assertThat(p).matches("$\\\\");
  }

  @Test
  public void atomLiteral() {
    Rule p = g.identifier;
    assertThat(p).matches("'find/me'");
    assertThat(p).matches("whatShallW3d0");
    assertThat(p).matches("nonode@nohost");
  }

  @Test
  public void stringLiteral() {
    Rule p = g.literal;

    assertThat(p).matches("\"\"");
    assertThat(p).matches("\"hello world\"");
    assertThat(p).matches("\"\\\"\"");
    assertThat(p).matches("\"\\\n\"");
    assertThat(p).matches("\"~n\"");
    assertThat(p).matches("\"a'b'c\"");
    assertThat(p).matches("\"a\\\"b\\\"c\"");
    assertThat(p).matches("\"This is a multiline string\nspanning two lines\"");
  }

  @Test
  public void booleanLiteral() {
    assertThat(g.identifier).matches("false");
    assertThat(g.identifier).matches("true");
  }

  @Test
  public void punctator() {
    assertThat(g.dollar).matches("$");
    assertThat(g.apostrophe).matches("'");
    assertThat(g.dot).matches(".");
    assertThat(g.comma).matches(",");
    assertThat(g.lparenthesis).matches("(");

    assertThat(g.dollar).matches("$");
    assertThat(g.apostrophe).matches("'");
    assertThat(g.dot).matches(".");
    assertThat(g.comma).matches(",");
    assertThat(g.lparenthesis).matches("(");
  }

  @Test
  public void custom() {
    assertThat(g.identifier).matches("module");
    assertThat(g.identifier).matches("m");
    assertThat(g.identifier).matches("_");
    assertThat(g.identifier).matches("ASDodule");
    assertThat(g.identifier).matches("A");
    assertThat(g.identifier).matches("Aodule");
  }

  @Test
  public void keyword() {
    assertThat(g.whenKeyword).matches("when");
    assertThat(g.borKeyword).matches("bor");
    assertThat(g.andalsoKeyword).matches("andalso");

    assertThat(g.keyword).matches("when");
    assertThat(g.keyword).matches("bor");
    assertThat(g.keyword).matches("andalso");
  }
}
