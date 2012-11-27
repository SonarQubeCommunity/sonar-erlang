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
package org.sonar.erlang.metrics;

import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.api.ErlangPunctuator;

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;

import java.util.List;

public class NumberOfFunctionArgument extends SquidCheck<ErlangGrammar> {

  List<ErlangPunctuator> nonArg = ImmutableList.of(ErlangPunctuator.LPARENTHESIS,
      ErlangPunctuator.RPARENTHESIS, ErlangPunctuator.COMMA);
  private ErlangGrammar grammar;

  @Override
  public void init() {
    grammar = getContext().getGrammar();
    subscribeTo(grammar.clauseHead);

  }

  @Override
  public void visitFile(AstNode astNode) {
  }

  @Override
  public void leaveFile(AstNode astNode) {
  }

  @Override
  public void visitNode(AstNode ast) {
    AstNode args = ast.findFirstDirectChild(grammar.funcDecl).findFirstDirectChild(
        grammar.arguments);
    int numOfArgs = 0;
    for (AstNode arg : args.getChildren()) {
      if (!nonArg.contains(arg.getType())) {
        numOfArgs++;
      }
    }
    getContext().peekSourceCode().add(ErlangMetric.NUM_OF_FUNC_ARGS, numOfArgs);

  }
}
