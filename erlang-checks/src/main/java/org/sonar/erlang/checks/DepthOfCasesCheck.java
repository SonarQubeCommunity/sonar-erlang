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
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

@Rule(key = "DepthOfCases", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE,
  name = "DepthOfCases", description = "Allowed maximum number of nested case statements")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class DepthOfCasesCheck extends SquidCheck<ErlangGrammar> {

  private static final int DEFAULT_MAXIMUM_CASE_DEPTH_THRESHOLD = 4;

  @RuleProperty(key = "maximumCaseDepthThreshold", defaultValue = ""
    + DEFAULT_MAXIMUM_CASE_DEPTH_THRESHOLD)
  private int maximumCaseDepthThreshold = DEFAULT_MAXIMUM_CASE_DEPTH_THRESHOLD;

  private ErlangGrammar g;

  @Override
  public void init() {
    g = getContext().getGrammar();
    subscribeTo(g.caseExpression);
  }

  @Override
  public void visitNode(AstNode astNode) {
    if (isTopLevelCase(astNode)) {
      int depth = countChild(astNode);
      if (depth > maximumCaseDepthThreshold) {
        getContext().createLineViolation(this,
            "Depth of case: {0} reached the threshold: {1}.", astNode.getTokenLine(),
            depth, maximumCaseDepthThreshold);
      }
    }

  }

  private boolean isTopLevelCase(AstNode astNode) {
    return !astNode.hasParents(g.caseExpression);
  }

  private int countChild(AstNode astNode) {
    return astNode.findChildren(g.caseExpression).size() - 1;
  }

}
