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

import org.apache.commons.lang.StringUtils;
import org.sonar.api.rules.RuleParam;
import org.sonar.api.rules.RulePriority;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.util.ArrayList;
import java.util.List;

public class XmlRuleHandler extends DefaultHandler {
  private final List<XmlRule> rules = new ArrayList<>();
  private XmlRule rule;
  private RuleParam param;
  private String value = "";

  public List<XmlRule> getRules() {
    return rules;
  }

  @Override
  public void startElement(String uri, String localName, String qName, Attributes attributes) {
    value = "";
    if (StringUtils.equalsIgnoreCase("rule", qName)) {
      rule = new XmlRule();
      param = null;
      rules.add(rule);
    } else if (StringUtils.equalsIgnoreCase("param", qName)) {
      if (rule != null) {
        param = rule.getRule().createParameter();
        param.setType(attributes.getValue("type"));
        param.setKey(attributes.getValue("key"));
        param.setDefaultValue(attributes.getValue("defaultValue"));
      }
    }
  }

  @Override
  public void endElement(String uri, String localName, String qName) {
    if (StringUtils.equalsIgnoreCase("rule", qName)) {
      rule = null;
      return;
    } else if (StringUtils.equalsIgnoreCase("param", qName)) {
      param = null;
      return;
    }
    if (rule == null) {
      return;
    }
    if (StringUtils.equalsIgnoreCase("name", qName)) {
      rule.getRule().setName(StringUtils.trim(value));
    } else if (StringUtils.equalsIgnoreCase("description", qName)) {
      if (param != null) {
        param.setDescription(StringUtils.trim(value));
      } else {
        rule.getRule().setDescription(StringUtils.trim(value));
      }
    } else if (StringUtils.equalsIgnoreCase("key", qName)) {
      rule.getRule().setKey(StringUtils.trim(value));
    } else if (StringUtils.equalsIgnoreCase("configKey", qName)) {
      rule.getRule().setConfigKey(StringUtils.trim(value));
    } else if (StringUtils.equalsIgnoreCase("priority", qName)) {
      rule.getRule().setSeverity(RulePriority.valueOf(StringUtils.trim(value)));
    } else if (StringUtils.equalsIgnoreCase("message", qName)) {
      rule.addMessage(StringUtils.trim(value));
    }
  }

  @Override
  public void characters(char ch[], int start, int length) {
    value = value + new String(ch, start, length);
  }

}