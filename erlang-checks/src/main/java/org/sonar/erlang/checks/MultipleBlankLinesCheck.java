/*
 * SonarQube Erlang Plugin
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

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.annotations.SqaleSubCharacteristic;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

@Rule(key = "MultipleBlankLines", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleSubCharacteristic(RulesDefinition.SubCharacteristics.READABILITY)
@SqaleConstantRemediation("1min")
public class MultipleBlankLinesCheck extends SquidCheck<LexerlessGrammar> implements AstAndTokenVisitor {

  @RuleProperty(key = "maxBlankLinesInsideFunctions", defaultValue = "1")
  public int maxBlankLinesInsideFunctions = 1;

  @RuleProperty(key = "maxBlankLinesOutsideFunctions", defaultValue = "2")
  public int maxBlankLinesOutsideFunctions = 2;

  private List<Integer> checkedLines = new ArrayList<Integer>();
  private boolean isInsideFunction = false;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.clauseBody);
  }

  @Override
  public void visitFile(@Nullable AstNode astNode) {
    checkedLines.clear();
    isInsideFunction = false;
  }

  @Override
  public void visitNode(AstNode astNode) {
    isInsideFunction = true;
  }

  @Override
  public void leaveNode(AstNode astNode) {
    isInsideFunction = false;
  }

  @Override
  public void visitToken(Token token) {
    if (!token.isGeneratedCode() && !checkedLines.contains(token.getLine())) {
      int previousLine = checkedLines.isEmpty() ? 0 : checkedLines.get(checkedLines.size() - 1);
      if (checkBlankLines(token, previousLine)) {
        getContext().createLineViolation(this,
          "Too many blank lines found, the threshold is {0}.",
          token.getLine(), getMaxFor(token));
      }
      checkedLines.add(token.getLine());
    }

  }

  private boolean compare(int line1, int line2, int comp) {
    return line1 - line2 - 1 > comp;
  }

  private int getMaxFor(Token token) {
    return (isInsideFunction) ? maxBlankLinesInsideFunctions : maxBlankLinesOutsideFunctions;
  }

  private boolean checkTrivias(int previousLine, Token token, int compTo) {
    int prevLine = previousLine;
    for (Trivia trivias : token.getTrivia()) {
      if (compare(trivias.getToken().getLine(), prevLine, compTo)) {
        return true;
      }
      prevLine = trivias.getToken().getLine();
    }
    return compare(token.getLine(), prevLine, compTo);
  }

  private boolean checkBlankLines(Token token, int previousLine) {
    int compTo = getMaxFor(token);

    boolean check = compare(token.getLine(), previousLine, compTo);
    if (check && token.hasTrivia()) {
      return checkTrivias(previousLine, token, compTo);
    }
    return check;
  }

}
