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
package org.sonar.plugins.erlang.xml;

import org.sonar.api.rules.Rule;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class XmlRule {
  private final List<Map.Entry<String, Pattern>> messages = new ArrayList<>();
  private final Rule sonarRule = Rule.create();

  XmlRule() {
    super();
  }

  boolean hasMessage(String message) {
    for (Map.Entry<String, Pattern> pair : messages) {
      Pattern p = pair.getValue();
      if (p != null) {
        if (p.matcher(message).matches()) {
          return true;
        }
      }
      String m = pair.getKey();
      if (m.equals(message)) {
        return true;
      }
    }
    return false;
  }

  void addMessage(String message) {
    String rxmessage = message.replaceAll("~[ws]", ".*?");
    rxmessage = rxmessage.replaceAll("([\\{\\}\\[\\]\\(\\)])", "\\\\$1");
    if (message.equals(rxmessage)) {
      messages.add(new AbstractMap.SimpleEntry<>(message, null));
    } else {
      Pattern pattern = Pattern.compile("^" + rxmessage + "$");
      messages.add(new AbstractMap.SimpleEntry<>(message, pattern));
    }
  }

  public Rule getRule() {
    return sonarRule;
  }

  public List<String> getMessages() {
    return messages.stream().map(pair -> pair.getKey()).collect(Collectors.toList());
  }

}
