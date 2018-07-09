/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2018 Tamas Kende; Denes Hegedus (Cursor Insight Ltd.)
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
package org.sonar.erlang.checks;

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;

import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "SpaceAfterBeforeOperators", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleConstantRemediation("2min")
public class SpaceAfterBeforeOperatorsCheck extends SquidCheck<LexerlessGrammar> {

  List<ErlangGrammarImpl> operators = ImmutableList.of(ErlangGrammarImpl.matchop,
    ErlangGrammarImpl.star, ErlangGrammarImpl.div, ErlangGrammarImpl.plus,
    ErlangGrammarImpl.minus);
  List<Integer> failedLines = new ArrayList<Integer>();

  private int numOfViolations = 0;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.primaryExpression);
  }

  @Override
  public void visitFile(@Nullable AstNode astNode) {
    numOfViolations = 0;
  }

  @Override
  public void visitNode(AstNode ast) {
    AstNode compTo;
    if (numOfViolations < 100 && !failedLines.contains(ast.getTokenLine())) {
      if (ast.getNextAstNode() != null && operators.contains(ast.getNextAstNode().getType())) {
        compTo = ast.getNextAstNode();
        failedLines.add(check(ast, compTo, false));
      } else if (ast.getPreviousAstNode() != null
        && operators.contains(ast.getPreviousAstNode().getType())) {
        compTo = ast.getPreviousAstNode();
        failedLines.add(check(ast, compTo, true));
      }
    }
  }

  private int check(AstNode ast, AstNode compTo, boolean previous) {
    int actCol = (previous) ? ast.getToken().getColumn() : ast.getLastToken().getColumn();
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
