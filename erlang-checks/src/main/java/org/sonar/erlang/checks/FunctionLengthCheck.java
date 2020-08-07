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

import com.sonar.sslr.api.AstNode;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.api.SourceFunction;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "FunctionLength", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleConstantRemediation("1h")
public class FunctionLengthCheck extends SquidCheck<LexerlessGrammar> {

  private static final int DEFAULT_MAXIMUM_FUNCTION_LENGTH_THRESHOLD = 20;

  @RuleProperty(key = "maximumFunctionLengthThreshold", defaultValue = ""
    + DEFAULT_MAXIMUM_FUNCTION_LENGTH_THRESHOLD)
  private int maximumFunctionLengthThreshold = DEFAULT_MAXIMUM_FUNCTION_LENGTH_THRESHOLD;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.functionClause);
  }

  @Override
  public void leaveNode(AstNode node) {
    SourceFunction function = (SourceFunction) getContext().peekSourceCode();
    int loc = function.getInt(ErlangMetric.LINES_OF_CODE);
    if (loc > maximumFunctionLengthThreshold) {
      getContext()
        .createLineViolation(
          this,
          "Function has {0,number,integer} lines of code which is greater than {1,number,integer} authorized.",
          node, loc,
          maximumFunctionLengthThreshold);
    }
  }

  public void setMaximumFunctionComplexityThreshold(int threshold) {
    this.maximumFunctionLengthThreshold = threshold;
  }

}
