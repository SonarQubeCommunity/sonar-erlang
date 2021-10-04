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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ErlangRuleManagerTest {
  XmlRuleManager ruleManager;

  @Before
  public void setUp() {
    ruleManager = new XmlRuleManager("/org/sonar/plugins/erlang/dialyzer/rules.xml");
  }

  @Test
  public void testGetRuleKeyByMessageIfExists() {
    Assert.assertEquals(
            "D019",
            ruleManager.getRuleKeyByMessage("Function will never be called")
    );
  }

  @Test
  public void testGetRuleKeyByMessageOtherRules() {
    Assert.assertEquals("OTHER_RULES",
            ruleManager.getRuleKeyByMessage("some nonexistent message"));
  }
}
