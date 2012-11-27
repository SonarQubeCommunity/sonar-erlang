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
import org.sonar.erlang.api.ErlangMetric;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.squid.api.SourceFunction;

@Rule(key = "FunctionClauses", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE,
  name = "FunctionClauses",
  description = "Check the maximum allowed number of function clauses")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NumOfFunctionClausesCheck extends SquidCheck<ErlangGrammar> {

  private static final int DEFAULT_MAXIMUM_FUNCTION_CLAUSES_THRESHOLD = 10;

  @RuleProperty(key = "maximumFunctionClausesThreshold", defaultValue = ""
    + DEFAULT_MAXIMUM_FUNCTION_CLAUSES_THRESHOLD)
  private int maximumFunctionClausesThreshold = DEFAULT_MAXIMUM_FUNCTION_CLAUSES_THRESHOLD;

  @Override
  public void init() {
    subscribeTo(getContext().getGrammar().functionDeclaration);
  }

  @Override
  public void leaveNode(AstNode node) {
    SourceFunction function = (SourceFunction) getContext().peekSourceCode();
    if (function.getInt(ErlangMetric.COMPLEXITY) > maximumFunctionClausesThreshold) {
      getContext()
          .createLineViolation(
              this,
              "Function has {0,number,integer} clauses which is greater than {1,number,integer} authorized.",
              node, function.getInt(ErlangMetric.COMPLEXITY),
              maximumFunctionClausesThreshold);
    }
  }

  public void setMaximumFunctionClausesThreshold(int threshold) {
    this.maximumFunctionClausesThreshold = threshold;
  }

}
