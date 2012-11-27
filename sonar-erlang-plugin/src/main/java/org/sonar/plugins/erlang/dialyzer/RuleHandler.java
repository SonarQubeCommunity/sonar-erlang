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

import org.apache.commons.lang.StringUtils;
import org.sonar.api.rules.RuleParam;
import org.sonar.api.rules.RulePriority;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.util.ArrayList;
import java.util.List;

public class RuleHandler extends DefaultHandler {
  private List<ErlangRule> rules = new ArrayList<ErlangRule>();
  private String tmpValue = "";
  private Object tmpRule;
  private RuleParam param;
  private boolean paramStarted = false;

  public List<ErlangRule> getRules() {
    return rules;
  }

  @Override
  public void startElement(String s, String s1, String elementName, Attributes attributes)
      throws SAXException {
    if ("rule".equals(elementName)) {
      rules.add(new ErlangRule());
      tmpRule = rules.get(rules.size() - 1);
      paramStarted = false;
    } else if (StringUtils.equalsIgnoreCase("param", elementName)) {
      param = ((ErlangRule) tmpRule).getRule().createParameter();
      param.setType(attributes.getValue("type"));
      param.setKey(attributes.getValue("key"));
      param.setDefaultValue(attributes.getValue("defaultValue"));
      paramStarted = true;
    } else {
      paramStarted = false;
    }
    tmpValue = "";
  }

  @Override
  public void endElement(String s, String s1, String element) throws SAXException {
    if (StringUtils.equalsIgnoreCase("name", element)) {
      ((ErlangRule) tmpRule).getRule().setName(StringUtils.trim(tmpValue));
    } else if (StringUtils.equalsIgnoreCase("description", element)) {
      if (paramStarted) {
        param.setDescription(StringUtils.trim(tmpValue));
      } else {
        ((ErlangRule) tmpRule).getRule().setDescription(StringUtils.trim(tmpValue));
      }
    } else if (StringUtils.equalsIgnoreCase("key", element)) {
      ((ErlangRule) tmpRule).getRule().setKey(StringUtils.trim(tmpValue));
    } else if (StringUtils.equalsIgnoreCase("configKey", element)) {
      ((ErlangRule) tmpRule).getRule().setConfigKey((StringUtils.trim(tmpValue)));
    } else if (StringUtils.equalsIgnoreCase("priority", element)) {
      ((ErlangRule) tmpRule).getRule().setSeverity(
          RulePriority.valueOf(StringUtils.trim(tmpValue)));
    } else if (StringUtils.equalsIgnoreCase("message", element)) {
      ((ErlangRule) tmpRule).addMessage(tmpValue.replaceAll("~.", ".*?").replaceAll(
          "([\\{\\}\\[\\]\\(\\)])", "\\\\$1"));
    }
  }

  @Override
  public void characters(char[] ac, int i, int j) throws SAXException {
    tmpValue = tmpValue + new String(ac, i, j);
  }

}
