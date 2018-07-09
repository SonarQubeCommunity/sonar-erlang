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

import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserLowLevelTest {

  private LexerlessGrammar b = ErlangGrammarImpl.createGrammar();

  @Test
  public void lexInlineComment() {
    assertThat(b.rule(ErlangGrammarImpl.spacing))
      .matches("% My Comment new line")
      .matches("%")
      .matches("%%%")
      .matches("%%%------------------------------\n%% asdasd");
  }

  @Test
  public void numericLiteral() {
    assertThat(b.rule(ErlangGrammarImpl.numericLiteral))
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
      .matches("$\\]")
      .matches("$\\")
      .matches("$w")
      .matches("$\\b")
      .matches("$\\123")
      .matches("$\\xA0")
      .matches("$\\x{A2F}")
      .matches("$\\^A")
      .matches("$\\\"")
      .matches("$\\\\");

  }

  @Test
  public void identifiers() {
    assertThat(b.rule(ErlangGrammarImpl.identifier))
      .matches("_")
      .matches("ASDodule")
      .matches("A")
      .matches("Aodule")
      .matches("__ASDASDASD")
      .notMatches("asdasdasdASDASDASD");

  }

  @Test
  public void atomLiteral() {
    assertThat(b.rule(ErlangGrammarImpl.atom))
      .matches("'find/me'")
      .matches("'orelse'")
      .matches("whatShallW3d0")
      .matches("nonode@nohost")
      .matches("lopikula")
      .matches("thi.is.an.atom")
      .notMatches("this.isnt.an.atom.")
      .matches("this@is.aswell")
      .matches("module")
      .matches("m");
  }

  @Test
  public void stringLiteral() {
    assertThat(b.rule(ErlangGrammarImpl.stringLiteral))
      .matches("\"\"")
      .matches("\"hello world\"")
      .matches("\"\\\"\"")
      .matches("\"\\\n\"")
      .matches("\"~n\"")
      .matches("\"a'b'c\"")
      .matches("\"a\\\"b\\\"c\"")
      .matches("\"This is a multiline string\nspanning two lines\"");
  }

  @Test
  public void booleanLiteral() {
    assertThat(b.rule(ErlangGrammarImpl.atom))
      .matches("false")
      .matches("true");
  }

  @Test
  public void punctator() {
    assertThat(b.rule(ErlangGrammarImpl.dollar))
      .matches("$");
    assertThat(b.rule(ErlangGrammarImpl.apostrophe))
      .matches("'");
    assertThat(b.rule(ErlangGrammarImpl.dot))
      .matches(".");
    assertThat(b.rule(ErlangGrammarImpl.comma))
      .matches(",");
    assertThat(b.rule(ErlangGrammarImpl.lparenthesis))
      .matches("(");
  }

  @Test
  public void keyword() {
    assertThat(b.rule(ErlangGrammarImpl.whenKeyword))
      .matches("when");
    assertThat(b.rule(ErlangGrammarImpl.borKeyword))
      .matches("bor");
    assertThat(b.rule(ErlangGrammarImpl.andalsoKeyword))
      .matches("andalso");
  }

  @Test
  public void arguments() {
    assertThat(b.rule(ErlangGrammarImpl.arguments))
      .matches("(add_mysql_connection,State)")
      .matches("(State#state.reg_name)")
      .matches("({query,Query},State)");
  }

}
