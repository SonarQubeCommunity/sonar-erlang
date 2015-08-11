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

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.annotations.SqaleSubCharacteristic;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.util.ArrayList;
import java.util.List;

@Rule(key = "NoSpaceAfterBeforeBrackets", priority = Priority.MAJOR)
/**
 * The rule generates a lot of false positives (record calls, etc). It is deactivated by default.
 */
//@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleSubCharacteristic(RulesDefinition.SubCharacteristics.READABILITY)
@SqaleConstantRemediation("1min")
public class NoSpaceAfterBeforeBracketsCheck extends SquidCheck<LexerlessGrammar> {

  List<ErlangGrammarImpl> noSpaceBefore = ImmutableList.of(ErlangGrammarImpl.rbracket,
    ErlangGrammarImpl.rcurlybrace, ErlangGrammarImpl.rparenthesis);
  List<ErlangGrammarImpl> noSpaceAfter = ImmutableList.of(ErlangGrammarImpl.lbracket,
    ErlangGrammarImpl.lcurlybrace, ErlangGrammarImpl.lparenthesis);
  List<Integer> failedLines = new ArrayList<Integer>();

  private int numOfViolations = 0;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.rbracket, ErlangGrammarImpl.rcurlybrace,
      ErlangGrammarImpl.rparenthesis, ErlangGrammarImpl.lbracket,
      ErlangGrammarImpl.lcurlybrace, ErlangGrammarImpl.lparenthesis);
  }

  @Override
  public void visitNode(AstNode ast) {
    Token compTo;
    if (numOfViolations < 100 && !failedLines.contains(ast.getTokenLine())) {
      if (ast.hasAncestor(ErlangGrammarImpl.clauseBody) && noSpaceAfter.contains(ast.getType())) {
        compTo = ast.getNextSibling().getToken();
        failedLines.add(check(ast, compTo, false));
      } else if (noSpaceBefore.contains(ast.getType())) {
        compTo = ast.getPreviousSibling().getLastToken();
        failedLines.add(check(ast, compTo, true));
      }
    }
  }

  private int check(AstNode ast, Token compTo, boolean previous) {
    /**
     * Ignore linebreaks
     */
    if (ast.getToken().getLine() != compTo.getLine()) {
      return -1;
    }
    int actCol = ast.getToken().getColumn();
    int actLength = ast.getTokenOriginalValue().length();
    int compCol = compTo.getColumn();
    int compLength = compTo.getOriginalValue().length();
    int actCheckPoint = (previous) ? actCol : actCol + actLength;
    int compCheckPoint = (previous) ? compCol + compLength : compCol;
    if (actCheckPoint != compCheckPoint) {
      getContext().createLineViolation(this, "Space after bracket in column: {0}.",
        ast.getToken().getLine(), actCol + 1);
      numOfViolations++;
      if (numOfViolations == 100) {
        getContext().createLineViolation(this,
          "File has reached 100 no space after/before brackets violation.",
          ast.getToken().getLine(), actCol + 1);
      }
      return ast.getToken().getLine();
    }
    return -1;
  }

}
