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

import org.sonar.erlang.api.ErlangGrammar;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import java.util.Iterator;
import java.util.List;

@Rule(key = "FunctionDefAndClausesSeparation", priority = Priority.MAJOR,
  cardinality = Cardinality.SINGLE, name = "FunctionDefAndClausesSeparation",
  description = "Rule for function declaration and clause separation")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class FunctionDefAndClausesSeparationCheck extends SquidCheck<ErlangGrammar> {

  @RuleProperty(key = "allowedBlankLinesBetweenClauses", defaultValue = "0")
  public int allowedBlankLinesBetweenClauses = 0;

  @RuleProperty(key = "allowedBlankLinesBetweenDefinitions", defaultValue = "1")
  public int allowedBlankLinesBetweenDefinitions = 1;

  private AstNode previousDefinition;

  @Override
  public void init() {
    subscribeTo(getContext().getGrammar().functionDeclaration);
  }

  @Override
  public void visitFile(AstNode astNode) {
  }

  @Override
  public void leaveFile(AstNode astNode) {
  }

  @Override
  public void visitNode(AstNode ast) {
    if (!ast.getToken().isGeneratedCode()) {
      /**
       * Check the definition first
       */
      if (ast.getType().equals(getContext().getGrammar().functionDeclaration)) {
        if (previousDefinition == null) {
          previousDefinition = ast;
        } else {
          check(ast, previousDefinition, allowedBlankLinesBetweenDefinitions);
          previousDefinition = ast;
        }
      }
      /**
       * Check the clauses
       */
      if (ast.findDirectChildren(getContext().getGrammar().functionClause).size() > 1) {
        List<AstNode> funcClauses = ast
            .findDirectChildren(getContext().getGrammar().functionClause);
        Iterator<AstNode> clauses = funcClauses.iterator();
        AstNode previousClause = clauses.next();
        while (clauses.hasNext()) {
          AstNode actClause = clauses.next();
          check(actClause, previousClause, allowedBlankLinesBetweenClauses);
          previousClause = actClause;

        }
      }
    }
  }

  private void check(AstNode ast, AstNode previous, int threshold) {
    if (diff(ast.getTokenLine(), previous.getLastToken().getLine(), threshold)) {
      boolean hasTrivias = ast.getToken().hasTrivia();
      if ((hasTrivias && checkTrivias(ast.getToken(), previous.getToken(), threshold))
        || !hasTrivias) {
        if (ast.getTokenLine() - previous.getLastToken().getLine() - 1 >= 0) {
          if (!ast.previousAstNode().equals(previous)) {
            check(ast, ast.previousAstNode(), 0);
          } else {
            getContext().createLineViolation(this,
                "The line has {0} precending blank line and it should be: {1}.",
                ast.getTokenLine(),
                (ast.getTokenLine() - previous.getLastToken().getLine() - 1),
                threshold);
          }
        }
      }
    }
  }

  private boolean diff(int a, int b, int threshold) {
    if (a - b - 1 != threshold) {
      return true;
    }
    return false;
  }

  private boolean checkTrivias(Token token, Token token2, int threshold) {
    int actLine = token2.getLine();
    for (Trivia trivia : token.getTrivia()) {
      if (actLine - trivia.getToken().getLine() - 1 > threshold) {
        return true;
      }
      actLine = trivia.getToken().getLine();
    }
    return false;
  }

}
