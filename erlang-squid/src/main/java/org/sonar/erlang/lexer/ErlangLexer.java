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

import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.commentRegexp;
import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.regexp;

public final class ErlangLexer {

    private static final String EXP = "([Ee][-]?+[0-9_]++)";
    private static final String escapeSequence = "(\\$\\\\b)|(\\$\\\\d)|(\\$\\\\e)|(\\$\\\\f)|(\\$\\\\n)|(\\$\\\\r)|(\\$\\\\s)|(\\$\\\\t)|(\\$\\\\v)|(\\$\\\\')|(\\$\\\\\")|(\\$\\\\\\\\)"
        + "|(\\$\\\\\\^[A-Za-z])"
        + "|(\\$\\\\x\\{[A-F0-9]+\\})"
        + "|(\\$\\\\x[A-F0-9]{1,2})"
        + "|(\\$\\\\[0-7]{1,3})";

    public static final String NUMERIC_LITERAL = "(?:"
        + "[0-9]++\\.([0-9]++)" + EXP + "?"
        + "|[0-9]++\\#([0-9A-Fa-f]++)?+"
        + "|[0-9]++"
        + "|" + escapeSequence
        + "|\\$[\\x00-\\x7F]"
        + ")";

    public static final String LITERAL = "(?:"
        + "\"([^\"\\\\]*+(\\\\[\\s\\S])?+)*+\")";

    public static final String COMMENT = "(?:"
        + "%[^\\n\\r]*+)";

    public static final String WHITESPACE = "[\\n\\r\\t\\u000B\\f\\u0020\\u00A0\\uFEFF\\p{Zs}]";

    public static final String IDENTIFIER = "('[^'\n\r]*')"
        + "|^(?!\\$)(\\p{javaJavaIdentifierStart}++[\\p{javaJavaIdentifierPart}@]*+)";

    private ErlangLexer() {
    }

    public static Lexer create(ErlangConfiguration conf) {
        return Lexer
                .builder()
                .withChannel(regexp(GenericTokenType.LITERAL, LITERAL))
                .withChannel(new BlackHoleChannel(WHITESPACE))
                .withChannel(commentRegexp(COMMENT))
                .withChannel(regexp(ErlangTokenType.NUMERIC_LITERAL, NUMERIC_LITERAL))
                .withChannel(new IdentifierAndKeywordChannel(IDENTIFIER,
                        true, ErlangKeyword.values()))
                .withChannel(new PunctuatorChannel(ErlangPunctuator.values()))
                .withChannel(new UnknownCharacterChannel(true))
                .build();
    }
}
