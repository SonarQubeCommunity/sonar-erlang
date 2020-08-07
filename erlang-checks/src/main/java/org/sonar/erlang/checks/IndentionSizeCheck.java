/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.sonar.erlang.checks;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "IndentionSize", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleConstantRemediation("1min")
public class IndentionSizeCheck extends SquidCheck<LexerlessGrammar> implements AstAndTokenVisitor {

  private Token previousToken;
  private int numOfViolations = 0;

  @RuleProperty(key = "regularExpression", defaultValue = "4")
  public int indentionSize = 4;

  @Override
  public void visitFile(AstNode astNode) {
    previousToken = null;
    numOfViolations = 0;
  }

  @Override
  public void leaveFile(AstNode astNode) {
    previousToken = null;
  }

  @Override
  public void visitToken(Token token) {
    if (numOfViolations < 100 && !token.isGeneratedCode() && (previousToken == null || previousToken.getLine() != token.getLine())){
      if (token.getColumn() % indentionSize != 0) {
        getContext()
          .createLineViolation(
            this,
            "The line starts with {0, number, integer} characters which is cannot be divided by {1, number, integer}.",
            token.getLine(), token.getColumn(), indentionSize);
        numOfViolations++;
        if (numOfViolations == 100) {
          getContext().createLineViolation(this,
            "File has reached 100 indention violation.", token.getLine(),
            token.getColumn(), indentionSize);
        }
      }
      previousToken = token;
    }
  }

}
