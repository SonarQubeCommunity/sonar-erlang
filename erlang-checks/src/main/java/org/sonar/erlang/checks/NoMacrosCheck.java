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

@Rule(key = "NoMacros", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE,
  name = "NoMacros", description = "Avoid using macros")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NoMacrosCheck extends SquidCheck<ErlangGrammar> {

  @RuleProperty(key = "skipDefineInFlowControl", defaultValue = "true",
    description = "Set it false if you want to check macros in flow controls.")
  private boolean skipDefineInFlowControl = true;

  private ErlangGrammar g;

  @Override
  public void init() {
    g = getContext().getGrammar();
    subscribeTo(g.defineAttr);
  }

  @Override
  public void visitNode(AstNode astNode) {
    if (!astNode.hasParents(g.flowControlAttr) || !skipDefineInFlowControl) {
      getContext().createLineViolation(this, "Do not use macros.", astNode.getTokenLine());
    }

  }

  public void setSkipDefineInFlowControl(boolean skipDefineInFlowControl) {
    this.skipDefineInFlowControl = skipDefineInFlowControl;
  }

}
