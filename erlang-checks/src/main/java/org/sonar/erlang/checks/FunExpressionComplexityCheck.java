/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 * Copyright © 2021 Daniils Petrovs <dpetrovs@evolution.com>
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
import org.sonar.squidbridge.checks.ChecksHelper;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "FunExpressionComplexity", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleConstantRemediation("1h")
public class FunExpressionComplexityCheck extends SquidCheck<LexerlessGrammar> {

  private static final int DEFAULT_MAXIMUM_FUN_EXPRESSION_COMPLEXITY_THRESHOLD = 4;

  @RuleProperty(key = "maximumFunExpressionComplexityThreshold", defaultValue = ""
    + DEFAULT_MAXIMUM_FUN_EXPRESSION_COMPLEXITY_THRESHOLD)
  private int maximumFunExpressionComplexityThreshold = DEFAULT_MAXIMUM_FUN_EXPRESSION_COMPLEXITY_THRESHOLD;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.funExpression);
  }

  @Override
  public void leaveNode(AstNode node) {
    SourceFunction function = (SourceFunction) getContext().peekSourceCode();
    int measuredComp = ChecksHelper.getRecursiveMeasureInt(function, ErlangMetric.COMPLEXITY);
    if (measuredComp > maximumFunExpressionComplexityThreshold) {
      getContext()
        .createLineViolation(
          this,
          "Function has a complexity of {0,number,integer} which is greater than {1,number,integer} authorized.",
          node, measuredComp,
          maximumFunExpressionComplexityThreshold);
    }
  }

  public void setMaximumFunExpressionComplexityThreshold(int threshold) {
    this.maximumFunExpressionComplexityThreshold = threshold;
  }

}
