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
import org.sonar.erlang.api.ErlangPunctuator;

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import java.util.ArrayList;
import java.util.List;

@Rule(key = "SpaceAfterBeforeOperators", priority = Priority.MAJOR,
  cardinality = Cardinality.SINGLE, name = "SpaceAfterBeforeOperators",
  description = "Space after and before operators")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class SpaceAfterBeforeOperatorsCheck extends SquidCheck<ErlangGrammar> {

  List<ErlangPunctuator> operators = ImmutableList.of(ErlangPunctuator.MATCHOP,
      ErlangPunctuator.STAR, ErlangPunctuator.DIV, ErlangPunctuator.PLUS,
      ErlangPunctuator.MINUS);
  List<Integer> failedLines = new ArrayList<Integer>();

  private int numOfViolations = 0;

  @Override
  public void init() {
    subscribeTo(getContext().getGrammar().primaryExpression);
  }

  @Override
  public void visitFile(AstNode astNode) {
  }

  @Override
  public void leaveFile(AstNode astNode) {
  }

  @Override
  public void visitNode(AstNode ast) {
    AstNode compTo;
    if (numOfViolations < 100 && !failedLines.contains(ast.getTokenLine())) {
      if (ast.nextSibling() != null && operators.contains(ast.nextSibling().getType())) {
        compTo = ast.nextSibling();
        failedLines.add(check(ast, compTo, false));
      } else if (ast.previousSibling() != null
        && operators.contains(ast.previousSibling().getType())) {
        compTo = ast.previousSibling();
        failedLines.add(check(ast, compTo, true));
      }
    }
  }

  private int check(AstNode ast, AstNode compTo, boolean previous) {
    int actCol = ast.getLastToken().getColumn();
    int actLength = ast.getTokenOriginalValue().length();
    int compCol = compTo.getToken().getColumn();
    int compLength = compTo.getTokenOriginalValue().length();
    int actCheckPoint = (previous) ? actCol - 1 : actCol + actLength + 1;
    int compCheckPoint = (previous) ? compCol + compLength : compCol;
    if (actCheckPoint != compCheckPoint) {
      getContext().createLineViolation(this, "No space after operator in column: {0}.",
          ast.getToken().getLine(), actCol + 1);
      numOfViolations++;
      if (numOfViolations == 100) {
        getContext().createLineViolation(this,
            "File has reached 100 'No space after operator' violation.",
            ast.getToken().getLine(), actCol + 1);
      }
      return ast.getToken().getLine();
    }
    return -1;
  }
}
