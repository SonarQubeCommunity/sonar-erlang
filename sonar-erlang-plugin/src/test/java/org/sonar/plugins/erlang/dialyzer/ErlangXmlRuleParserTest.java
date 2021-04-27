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
package org.sonar.plugins.erlang.dialyzer;

import org.junit.Assert;
import org.junit.Test;
import org.sonar.api.rules.Rule;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.List;

public class ErlangXmlRuleParserTest {
  private final File dialyzerRuleFile = new File("src/test/resources/org/sonar/plugins/erlang/dialyzer/rules.xml");

  @Test
  public void testParseSuccess() {
    try {
      InputStream in = new FileInputStream(dialyzerRuleFile);

      ErlangXmlRuleParser parser = new ErlangXmlRuleParser();
      List<ErlangRule> rules = parser.parse(in);

      Rule rule = rules.get(0).getRule();

      Assert.assertEquals(
              "X001",
              rule.getKey()
      );

      Assert.assertEquals(
              "Undefined function calls",
              rule.getName()
      );

      Assert.assertEquals(
              "Undefined function calls",
              rule.getDescription()
      );

    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }
  }
}
