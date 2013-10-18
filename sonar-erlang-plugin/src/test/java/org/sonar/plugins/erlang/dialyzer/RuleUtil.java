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
package org.sonar.plugins.erlang.dialyzer;

import org.sonar.api.rules.ActiveRule;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RulePriority;
import org.sonar.check.Cardinality;

public class RuleUtil {

  public static ActiveRule generateActiveRule(String ruleName, String ruleKey) {
    Rule rule = Rule.create();
    rule.setName(ruleName);
    rule.setKey(ruleKey);
    rule.setConfigKey(ruleKey);
    rule.setRepositoryKey("Erlang");
    rule.setStatus(Rule.STATUS_READY);
    rule.setSeverity(RulePriority.MAJOR);
    rule.setCardinality(Cardinality.SINGLE);
    ActiveRule activeRule = new ActiveRule();
    activeRule.setSeverity(RulePriority.MAJOR);
    activeRule.setRule(rule);
    return activeRule;
  }

}
