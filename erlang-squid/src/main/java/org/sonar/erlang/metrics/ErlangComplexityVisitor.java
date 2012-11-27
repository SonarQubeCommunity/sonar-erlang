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
import com.sonar.sslr.api.AstNodeType;
import com.sonar.sslr.squid.SquidAstVisitor;

/**
 * Erlang complexity calculation +1 for every function clause over 1 +1 for
 * every fun expression +1 for every branch expression (if) +1 for every pattern
 * statement (case, try) +1 for every catch pattern statement (catch)
 * 
 * @author Tamas Kende
 * 
 */
public class ErlangComplexityVisitor extends SquidAstVisitor<ErlangGrammar> {

  private AstNodeType[] complexityAstNodeType;
  ErlangGrammar grammar;

  @Override
  public void init() {
    grammar = getContext().getGrammar();
    complexityAstNodeType = new AstNodeType[] {
      // Entry points
      grammar.functionDeclaration, grammar.funExpression,

      // Branching nodes
      grammar.branchExp, grammar.patternStatement, grammar.catchPatternStatement,

        // TODO: Expressions? when increase? if there are more than one guard
        // and more than
        // one guard expression?
        // grammar.guardSequence
    };
    subscribeTo(complexityAstNodeType);
  }

  @Override
  public void visitNode(AstNode astNode) {
    if (astNode.getType().equals(grammar.functionDeclaration)) {
      countClauses(astNode);
    } else {
      increaseComplexity(1);
    }

  }

  private void increaseComplexity(int i) {
    getContext().peekSourceCode().add(ErlangMetric.COMPLEXITY, i);
  }

  /**
   * Increase the complexity with the number of clauses
   * 
   * @param functionDec
   */
  private void countClauses(AstNode functionDec) {
    increaseComplexity((functionDec.findDirectChildren(grammar.functionClause).size()));
  }

}
