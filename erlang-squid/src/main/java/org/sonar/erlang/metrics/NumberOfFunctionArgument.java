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
package org.sonar.erlang.metrics;

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;

import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.util.List;

public class NumberOfFunctionArgument extends SquidCheck<LexerlessGrammar> {

  List<ErlangGrammarImpl> nonArg = ImmutableList.of(ErlangGrammarImpl.lparenthesis,
    ErlangGrammarImpl.rparenthesis, ErlangGrammarImpl.comma);

  @Override
  public void init() {

    subscribeTo(ErlangGrammarImpl.clauseHead);

  }

  @Override
  public void visitNode(AstNode ast) {
    AstNode args = ast.getFirstChild(ErlangGrammarImpl.funcDecl).getFirstChild(
      ErlangGrammarImpl.arguments);
    int numOfArgs = 0;
    for (AstNode arg : args.getChildren()) {
      if (!nonArg.contains(arg.getType())) {
        numOfArgs++;
      }
    }
    getContext().peekSourceCode().add(ErlangMetric.NUM_OF_FUNC_ARGS, numOfArgs);

  }

}
