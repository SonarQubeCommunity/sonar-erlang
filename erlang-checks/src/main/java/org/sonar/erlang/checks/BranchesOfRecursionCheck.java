/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2016 Tamas Kende
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

import com.sonar.sslr.api.AstNode;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.annotations.SqaleSubCharacteristic;
import org.sonar.squidbridge.api.SourceFunction;
import org.sonar.squidbridge.checks.ChecksHelper;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "BranchesOfRecursion", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleSubCharacteristic(RulesDefinition.SubCharacteristics.LOGIC_CHANGEABILITY)
@SqaleConstantRemediation("1h")
public class BranchesOfRecursionCheck extends SquidCheck<LexerlessGrammar> {

  private static final int DEFAULT_MAXIMUM_BOR_THRESHOLD = 10;

  @RuleProperty(key = "maximumBORThreshold", defaultValue = "" + DEFAULT_MAXIMUM_BOR_THRESHOLD)
  private int maximumBORThreshold = DEFAULT_MAXIMUM_BOR_THRESHOLD;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.functionDeclaration);
  }

  @Override
  public void leaveNode(AstNode node) {
    SourceFunction function = (SourceFunction) getContext().peekSourceCode();

    int measuredBOR = ChecksHelper.getRecursiveMeasureInt(function, ErlangMetric.BRANCHES_OF_RECURSION);
    if (measuredBOR > maximumBORThreshold) {
      getContext()
        .createLineViolation(
          this,
          "Function has {0,number,integer} branches of recursion which is greater than {1,number,integer} authorized.",
          node, measuredBOR,
          maximumBORThreshold);
    }
  }

  public void setMaximumBORThreshold(int threshold) {
    this.maximumBORThreshold = threshold;
  }

}
