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

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.squid.checks.SquidCheck;

import java.util.List;

public class BranchesOfRecursion extends SquidCheck<ErlangGrammar> {

  private ErlangGrammar grammar;

  @Override
  public void init() {
    grammar = getContext().getGrammar();
    subscribeTo(grammar.functionDeclaration);

  }

  @Override
  public void visitFile(AstNode astNode) {
  }

  @Override
  public void leaveFile(AstNode astNode) {
  }

  @Override
  public void leaveNode(AstNode ast) {
    List<AstNode> functionClauses = ast.findDirectChildren(grammar.functionClause);
    String functionName = ast.findFirstDirectChild(grammar.functionClause)
        .findFirstDirectChild(grammar.clauseHead).findFirstDirectChild(grammar.funcDecl)
        .findFirstDirectChild(GenericTokenType.IDENTIFIER).getTokenOriginalValue();
    int numOfRecursion = 0;
    for (AstNode functionClause : functionClauses) {
      List<AstNode> calls = functionClause.findChildren(grammar.callExpression);
      for (AstNode call : calls) {
        if (call.findFirstDirectChild(grammar.primaryExpression).getTokenOriginalValue()
            .equals(functionName)) {
          numOfRecursion++;
        }

      }
    }
    // getContext().popSourceCode()
    getContext().peekSourceCode().add(ErlangMetric.BRANCHES_OF_RECURSION, numOfRecursion);
  }

  @Override
  public void visitNode(AstNode ast) {

  }
}
