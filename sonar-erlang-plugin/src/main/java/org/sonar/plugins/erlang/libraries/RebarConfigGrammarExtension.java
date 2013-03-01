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
package org.sonar.plugins.erlang.libraries;

import org.sonar.sslr.grammar.GrammarRuleKey;
import org.sonar.sslr.grammar.LexerlessGrammarBuilder;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.erlang.parser.ErlangGrammarImpl.statements;
import static org.sonar.erlang.parser.ErlangGrammarImpl.statement;
import static org.sonar.erlang.parser.ErlangGrammarImpl.dot;
import static org.sonar.erlang.parser.ErlangGrammarImpl.spacing;
import static org.sonar.erlang.parser.ErlangGrammarImpl.eof;

public enum RebarConfigGrammarExtension implements GrammarRuleKey {

  rebarConfig;

  public static LexerlessGrammarBuilder createGrammarBuilder(LexerlessGrammarBuilder basic) {
    LexerlessGrammarBuilder extended = LexerlessGrammarBuilder.createBasedOn(basic);
    extended.rule(statements).override(statement, extended.zeroOrMore(dot, statement));
    extended.rule(rebarConfig).override(spacing, extended.optional(statements));
    extended.setRootRule(rebarConfig);
    return extended;
  }

  public static LexerlessGrammar createGrammar(LexerlessGrammarBuilder basic) {
    return createGrammarBuilder(basic).build();
  }


}
