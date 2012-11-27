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

import static org.sonar.erlang.api.ErlangTokenType.NUMERIC_LITERAL;

import com.sonar.sslr.impl.Lexer;
import com.sonar.sslr.impl.channel.BlackHoleChannel;
import com.sonar.sslr.impl.channel.IdentifierAndKeywordChannel;
import com.sonar.sslr.impl.channel.PunctuatorChannel;
import com.sonar.sslr.impl.channel.UnknownCharacterChannel;

import static com.sonar.sslr.api.GenericTokenType.LITERAL;
import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.commentRegexp;
import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.or;
import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.regexp;

public final class ErlangLexer {

  private static final String EXP = "([Ee][-]?+[0-9_]++)";
  private static final String escapeSequence = "(\\$\\\\b)|(\\$\\\\d)|(\\$\\\\e)|(\\$\\\\f)|(\\$\\\\n)|(\\$\\\\r)|(\\$\\\\s)|(\\$\\\\t)|(\\$\\\\v)|(\\$\\\\')|(\\$\\\\\")|(\\$\\\\\\\\)"
    + "|(\\$\\\\\\^[A-Za-z])"
    + "|(\\$\\\\x\\{[A-F0-9]+\\})"
    + "|(\\$\\\\x[A-F0-9]{1,2})"
    + "|(\\$\\\\[0-7]{1,3})";

  private ErlangLexer() {
  }

  public static Lexer create(ErlangConfiguration conf) {
    return Lexer
        .builder()
        .withChannel(regexp(LITERAL, "\"([^\"\\\\]*+(\\\\[\\s\\S])?+)*+\""))
        .withChannel(new BlackHoleChannel("\\s++"))
        .withChannel(commentRegexp("%[^\\n\\r]*+"))
        .withChannel(regexp(NUMERIC_LITERAL, "[0-9]++\\.([0-9]++)" + EXP + "?"))
        .withChannel(regexp(NUMERIC_LITERAL, "[0-9]++\\#([0-9A-Fa-f]++)?+"))
        .withChannel(regexp(NUMERIC_LITERAL, "[0-9]++"))
        .withChannel(regexp(NUMERIC_LITERAL, escapeSequence))
        .withChannel(regexp(NUMERIC_LITERAL, "\\$[\\x00-\\x7F]"))
        .withChannel(
            new IdentifierAndKeywordChannel(
                or("('[^'\n\r]*')",
                    "^(?!\\$)(\\p{javaJavaIdentifierStart}++[\\p{javaJavaIdentifierPart}@]*+)"),
                true, ErlangKeyword.values())).withChannel(
            new PunctuatorChannel(ErlangPunctuator.values())).withChannel(
            new UnknownCharacterChannel(true)).build();
  }

}
