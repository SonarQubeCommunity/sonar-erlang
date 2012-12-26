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

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.api.ErlangPunctuator;

public class BranchesOfRecursion extends SquidCheck<ErlangGrammar> {

  private ErlangGrammar grammar;
  private String actualArity;
  private String actualModule;

  @Override
  public void init() {
    grammar = getContext().getGrammar();
    subscribeTo(grammar.functionDeclaration, grammar.callExpression);

  }

  @Override
  public void visitFile(AstNode astNode) {
    if (astNode != null) {
      actualArity = "";
      actualModule = astNode.findFirstChild(grammar.moduleAttr)
          .findFirstDirectChild(GenericTokenType.IDENTIFIER).getTokenOriginalValue();
    }
  }

  @Override
  public void visitNode(AstNode ast) {
    if (ast.getType().equals(grammar.functionDeclaration)) {
      actualArity = getArity(ast.findFirstDirectChild(grammar.functionClause));
    }
    if (ast.getType().equals(grammar.callExpression)) {
      if (getArityFromCall(ast).equals(actualArity)) {
        getContext().peekSourceCode().add(ErlangMetric.BRANCHES_OF_RECURSION, 1);
      }
    }
  }

  private String getArityFromCall(AstNode ast) {
    // It has a colon, so it is a module:function call
    if (ast.hasDirectChildren(ErlangPunctuator.COLON)) {
      if (actualModule.equals(ast.getChild(0).getTokenOriginalValue())) {
        return ast.getChild(2).getTokenOriginalValue() + "/" + getNumOfArgs(ast.findFirstDirectChild(grammar.arguments));
      }
      return ast.getChild(0) + ":" + ast.getChild(2).getTokenOriginalValue() + "/" + getNumOfArgs(ast.findFirstDirectChild(grammar.arguments));
    } else {
      return ast.findFirstDirectChild(grammar.primaryExpression).findFirstDirectChild(grammar.literal).getTokenOriginalValue() + "/"
        + getNumOfArgs(ast.findFirstDirectChild(grammar.arguments));
    }
  }

  private String getArity(AstNode ast) {
    AstNode args = ast.findFirstDirectChild(grammar.clauseHead)
        .findFirstDirectChild(grammar.funcDecl).findFirstDirectChild(
            grammar.arguments);
    return ast.getTokenOriginalValue() + "/" + getNumOfArgs(args);
  }

  private String getNumOfArgs(AstNode args) {
    int num = args.getNumberOfChildren() > 3 ? args.findDirectChildren(
        ErlangPunctuator.COMMA).size() + 1 : args.getNumberOfChildren() - 2;
    return String.valueOf(num);
  }

}
