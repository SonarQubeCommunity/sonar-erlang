/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2018 Tamas Kende; Denes Hegedus (Cursor Insight Ltd.)
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

import org.sonar.api.rules.Rule;

import java.util.ArrayList;
import java.util.List;

public class ErlangRule {
  private List<String> messages = new ArrayList<>();
  private Rule sonarRule = Rule.create();

  ErlangRule() {
    super();
  }

  boolean hasMessage(String message) {
    boolean ret = false;
    for (String ruleMessage : messages) {
      if (message.matches(ruleMessage)) {
        ret = true;
        break;
      }
    }
    return ret;
  }

  void addMessage(String message) {
    messages.add(message);
  }

  public Rule getRule() {
    return sonarRule;
  }

}
