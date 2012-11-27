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

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

@Rule(key = "IndentionSize", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE,
  name = "IndentionSize", description = "Allowed indention size")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class IndentionSizeCheck extends SquidCheck<ErlangGrammar> implements AstAndTokenVisitor {

  private Token previousToken;
  private int numOfViolations = 0;

  @RuleProperty(key = "regularExpression", defaultValue = "4")
  public int indentionSize = 4;

  @Override
  public void visitFile(AstNode astNode) {
    previousToken = null;
  }

  @Override
  public void leaveFile(AstNode astNode) {
    previousToken = null;
  }

  public void visitToken(Token token) {
    if (numOfViolations < 100 && !token.isGeneratedCode()) {
      if (previousToken == null
        || (previousToken != null && previousToken.getLine() != token.getLine())) {
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

}
