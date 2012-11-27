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
package org.sonar.erlang.lexer;

import org.sonar.erlang.ErlangConfiguration;
import org.sonar.erlang.api.ErlangKeyword;
import org.sonar.erlang.api.ErlangPunctuator;
import org.sonar.erlang.api.ErlangTokenType;
import org.sonar.erlang.lexer.ErlangLexer;

import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.impl.Lexer;
import com.sonar.sslr.impl.channel.UnknownCharacterChannel;
import org.hamcrest.Matchers;
import org.junit.BeforeClass;
import org.junit.Test;

import java.nio.charset.Charset;

import static com.sonar.sslr.test.lexer.LexerMatchers.hasComment;
import static com.sonar.sslr.test.lexer.LexerMatchers.hasToken;
import static com.sonar.sslr.test.lexer.LexerMatchers.hasTokens;
import static org.junit.Assert.assertThat;

public class ErlangLexerTest {

  private static Lexer lexer;

  @BeforeClass
  public static void init() {
    lexer = ErlangLexer.create(new ErlangConfiguration(Charset.forName("UTF-8")));
  }

  @Test
  public void lexInlineComment() {
    assertThat(lexer.lex("% My Comment \n new line"), hasComment("% My Comment "));
    assertThat(lexer.lex("%"), hasComment("%"));
  }

  @Test
  public void decimalLiteral() {
    assertThat(lexer.lex("0"), hasToken("0", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("123"), hasToken("123", ErlangTokenType.NUMERIC_LITERAL));

    assertThat(lexer.lex("123.456"), hasToken("123.456", ErlangTokenType.NUMERIC_LITERAL));

    assertThat(lexer.lex("123.456e10"), hasToken("123.456e10", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("123.456e-10"), hasToken("123.456e-10",
        ErlangTokenType.NUMERIC_LITERAL));

    assertThat(lexer.lex("123.456E10"), hasToken("123.456E10", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("123.456E-10"), hasToken("123.456E-10",
        ErlangTokenType.NUMERIC_LITERAL));

    assertThat(lexer.lex("0.123"), hasToken("0.123", ErlangTokenType.NUMERIC_LITERAL));

    assertThat(lexer.lex("0.123e4"), hasToken("0.123e4", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("0.123e-4"), hasToken("0.123e-4", ErlangTokenType.NUMERIC_LITERAL));

    assertThat(lexer.lex("0.123E4"), hasToken("0.123E4", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("0.123E-4"), hasToken("0.123E-4", ErlangTokenType.NUMERIC_LITERAL));

    assertThat(lexer.lex("2#12"), hasToken("2#12", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("16#1f"), hasToken("16#1f", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\n"), hasToken("$\\n", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\"), hasToken("$\\", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$w"), hasToken("$w", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\b"), hasToken("$\\b", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\123"), hasToken("$\\123", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\xA0"), hasToken("$\\xA0", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\x{A2F}"), hasToken("$\\x{A2F}", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\^A"), hasToken("$\\^A", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\\""), hasToken("$\\\"", ErlangTokenType.NUMERIC_LITERAL));
    assertThat(lexer.lex("$\\\\"), hasToken("$\\\\", ErlangTokenType.NUMERIC_LITERAL));
  }

  @Test
  public void atomLiteral() {
    assertThat(lexer.lex("'find/me'"), hasToken("'find/me'", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("whatShallW3d0"), hasToken("whatShallW3d0",
        GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("nonode@nohost"), hasToken("nonode@nohost",
        GenericTokenType.IDENTIFIER));
  }

  @Test
  public void stringLiteral() {
    assertThat("empty", lexer.lex("\"\""), hasToken("\"\"", GenericTokenType.LITERAL));
    assertThat(lexer.lex("\"hello world\""), hasToken("\"hello world\"",
        GenericTokenType.LITERAL));
    assertThat("escaped double quote", lexer.lex("\"\\\"\""), hasToken("\"\\\"\"",
        GenericTokenType.LITERAL));
    assertThat("multiline", lexer.lex("\"\\\n\""), hasToken("\"\\\n\"",
        GenericTokenType.LITERAL));
    assertThat("new line in literal", lexer.lex("\"~n\""), hasToken("\"~n\"",
        GenericTokenType.LITERAL));
    assertThat("new line in literal", lexer.lex("\"a'b'c\""), hasToken("\"a'b'c\"",
        GenericTokenType.LITERAL));
    assertThat("new line in literal", lexer.lex("\"a\\\"b\\\"c\""), hasToken("\"a\\\"b\\\"c\"",
        GenericTokenType.LITERAL));
    assertThat("new line in literal", lexer
        .lex("\"This is a multiline string\nspanning two lines\""), hasToken(
        "\"This is a multiline string\nspanning two lines\"", GenericTokenType.LITERAL));
  }

  @Test
  public void booleanLiteral() {
    assertThat(lexer.lex("false"), hasToken("false", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("true"), hasToken("true", GenericTokenType.IDENTIFIER));
  }

  @Test
  public void punctator() {
    assertThat(lexer.lex("$"), hasToken("$", ErlangPunctuator.DOLLAR));
    assertThat(lexer.lex("'"), hasToken("'", ErlangPunctuator.APOSTROPHE));
    assertThat(lexer.lex("."), hasToken(".", ErlangPunctuator.DOT));
    assertThat(lexer.lex(","), hasToken(",", ErlangPunctuator.COMMA));
    assertThat(lexer.lex("("), hasToken("(", ErlangPunctuator.LPARENTHESIS));
  }

  @Test
  public void custom() {
    assertThat(lexer.lex("module"), hasToken("module", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("m"), hasToken("m", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("_"), hasToken("_", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("ASDodule"), hasToken("ASDodule", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("A"), hasToken("A", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("Aodule"), hasToken("Aodule", GenericTokenType.IDENTIFIER));
    assertThat(lexer.lex("1."), Matchers.allOf(hasToken("1", ErlangTokenType.NUMERIC_LITERAL),
        hasToken(".", ErlangPunctuator.DOT)));

  }

  @Test
  public void keyword() {
    assertThat(lexer.lex("when"), hasToken("when", ErlangKeyword.WHEN));
    assertThat(lexer.lex("bor"), hasToken("bor", ErlangKeyword.BOR));
    assertThat(lexer.lex("andalso"), hasToken("andalso", ErlangKeyword.ANDALSO));
  }

  @Test
  public void bom() {
    assertThat(lexer.lex(Character.toString(UnknownCharacterChannel.BOM_CHAR)),
        hasTokens("EOF"));
  }

}
