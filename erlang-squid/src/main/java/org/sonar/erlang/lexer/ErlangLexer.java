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

import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.impl.Lexer;
import com.sonar.sslr.impl.channel.BlackHoleChannel;
import com.sonar.sslr.impl.channel.IdentifierAndKeywordChannel;
import com.sonar.sslr.impl.channel.PunctuatorChannel;
import com.sonar.sslr.impl.channel.UnknownCharacterChannel;
import org.sonar.erlang.ErlangConfiguration;
import org.sonar.erlang.api.ErlangKeyword;
import org.sonar.erlang.api.ErlangPunctuator;
import org.sonar.erlang.api.ErlangTokenType;
import org.sonar.erlang.parser.ErlangGrammarImpl;

import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.commentRegexp;
import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.regexp;

public final class ErlangLexer {

  private ErlangLexer() {
  }

  public static Lexer create(ErlangConfiguration conf) {
    return Lexer
        .builder()
        .withChannel(regexp(GenericTokenType.LITERAL, ErlangGrammarImpl.LITERAL))
        .withChannel(new BlackHoleChannel(ErlangGrammarImpl.WHITESPACE))
        .withChannel(commentRegexp(ErlangGrammarImpl.COMMENT))
        .withChannel(regexp(ErlangTokenType.NUMERIC_LITERAL, ErlangGrammarImpl.NUMERIC_LITERAL))
        .withChannel(new IdentifierAndKeywordChannel(ErlangGrammarImpl.IDENTIFIER,
            true, ErlangKeyword.values()))
        .withChannel(new PunctuatorChannel(ErlangPunctuator.values()))
        .withChannel(new UnknownCharacterChannel(true))
        .build();
  }
}
