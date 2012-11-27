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
import org.sonar.api.rules.RuleParam;
import org.sonar.api.rules.RulePriority;
import org.sonar.check.Cardinality;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class RuleUtil {

  public static ErlangRule getOneRule(String key, String repoKey) {
    ErlangRuleManager rm = new ErlangRuleManager(DialyzerRuleRepository.DIALYZER_PATH);
    return rm.getErlangRuleByKey(key);
  }

  public static ActiveRule generateActiveRule(String ruleName, String ruleKey,
      Map<String, String> parameters) {
    List<RuleParam> params = new ArrayList<RuleParam>();
    Rule rule = new Rule();
    if (parameters != null && parameters.size() > 0) {
      for (Entry<String, String> p : parameters.entrySet()) {
        RuleParam param = new RuleParam();
        param.setKey(p.getKey());
        param.setDefaultValue(p.getValue());
        params.add(param);

      }
      rule.setParams(params);
    }
    rule.setName(ruleName);
    rule.setKey(ruleKey);
    rule.setConfigKey(ruleKey);
    rule.setPluginName("Erlang");
    rule.setEnabled(true);
    rule.setSeverity(RulePriority.MAJOR);
    rule.setCardinality(Cardinality.SINGLE);
    ActiveRule activeRule = new ActiveRule();
    activeRule.setPriority(RulePriority.MAJOR);
    activeRule.setRule(rule);
    if (parameters != null && parameters.size() > 0) {
      for (Entry<String, String> p : parameters.entrySet()) {
        activeRule.setParameter(p.getKey(), p.getValue());
      }
    }
    return activeRule;
  }

  public static Rule generateRule(String ruleName, String ruleKey, Map<String, String> parameters) {
    return generateActiveRule(ruleName, ruleKey, parameters).getRule();
  }

}
