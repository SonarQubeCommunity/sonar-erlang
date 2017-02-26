/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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
package org.sonar.plugins.erlang.dialyzer;

import java.util.ArrayList;
import java.util.List;

public class ErlangRuleManager  {

  private List<ErlangRule> rules = new ArrayList<ErlangRule>();

  private static final String OTHER_RULES_KEY = "OTHER_RULES";
  public static final String UNUSED_NAMES_KEY = "UNUSED_NAMES";

  ErlangRuleManager(String rulesPath) {
    rules = new ErlangXmlRuleParser().parse(ErlangRuleManager.class
      .getResourceAsStream(rulesPath));
  }

  String getRuleKeyByMessage(String message) {
    for (ErlangRule rule : rules) {
      if (rule.hasMessage(message)) {
        return rule.getRule().getKey();
      }
    }
    return OTHER_RULES_KEY;
  }

}
