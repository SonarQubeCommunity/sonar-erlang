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
package org.sonar.erlang.checks;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangPunctuator;

@Rule(key = "IsTailRecursive", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
public class IsTailRecursiveCheck extends SquidCheck<ErlangGrammar> {

  private String actualArity;
  private String actualModule;
  private ErlangGrammar grammar;
  private int lastClauseLine;

  @Override
  public void init() {
    grammar = getContext().getGrammar();
    subscribeTo(getContext().getGrammar().callExpression, getContext().getGrammar().functionDeclaration);
  }

  @Override
  public void visitFile(AstNode astNode) {
    if (astNode == null) {
      // file wasn't parsed
      return;
    }
    actualArity = "";
    actualModule = astNode.getFirstDescendant(grammar.moduleAttr)
        .getFirstChild(GenericTokenType.IDENTIFIER).getTokenOriginalValue();
    lastClauseLine = 0;
  }

  @Override
  public void visitNode(AstNode node) {
    if (node.getType().equals(grammar.functionDeclaration)) {
      actualArity = getArity(node.getFirstChild(grammar.functionClause));
    }
    if (node.getType().equals(grammar.callExpression)
      /**
       * Recursive call where we have not record a non tail recursive call
       */
      && (getArityFromCall(node).equals(actualArity) && node.getFirstAncestor(grammar.functionClause).getTokenLine() != lastClauseLine)) {
      /**
       * Not a standalone statement
       */
      if (!node.getParent().getType().equals(grammar.expression)) {
        getContext().createLineViolation(this, "Function is not tail recursive.", node);
        lastClauseLine = node.getFirstAncestor(grammar.functionClause).getTokenLine();
        return;
      }

      /**
       * Not last call
       */
      if (!checkIsLastStatement(node.getFirstAncestor(grammar.statement))) {
        getContext().createLineViolation(this, "Function is not tail recursive.", node);
        lastClauseLine = node.getFirstAncestor(grammar.functionClause).getTokenLine();
        return;
      }

    }
  }

  private boolean checkIsLastStatement(AstNode node) {
    if (node == null) {
      return true;
    }
    AstNode sibling = node.getNextSibling();
    if (sibling != null) {
      return false;
    }
    return checkIsLastStatement(node.getFirstAncestor(grammar.statement));
  }

  private String getArityFromCall(AstNode ast) {
    // It has a colon, so it is a module:function call
    if (ast.hasDirectChildren(ErlangPunctuator.COLON)) {
      if (actualModule.equals(ast.getChild(0).getTokenOriginalValue())) {
        return ast.getChild(2).getTokenOriginalValue() + "/" + getNumOfArgs(ast.getFirstChild(grammar.arguments));
      }
      return ast.getChild(0) + ":" + ast.getChild(2).getTokenOriginalValue() + "/" + getNumOfArgs(ast.getFirstChild(grammar.arguments));
    } else {
      return ast.getFirstChild(grammar.primaryExpression).getFirstChild(grammar.literal).getTokenOriginalValue() + "/"
        + getNumOfArgs(ast.getFirstChild(grammar.arguments));
    }
  }

  private String getArity(AstNode ast) {
    AstNode args = ast.getFirstChild(grammar.clauseHead)
        .getFirstChild(grammar.funcDecl).getFirstChild(
            grammar.arguments);
    return ast.getTokenOriginalValue() + "/" + getNumOfArgs(args);
  }

  private String getNumOfArgs(AstNode args) {
    int num = args.getNumberOfChildren() > 3 ? args.getChildren(
        ErlangPunctuator.COMMA).size() + 1 : args.getNumberOfChildren() - 2;
    return String.valueOf(num);
  }

}
