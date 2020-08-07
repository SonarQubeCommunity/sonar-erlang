/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
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
package org.sonar.plugins.erlang.checks;

import java.util.List;

import org.junit.Test;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.erlang.checks.CheckList;

import static org.fest.assertions.Assertions.assertThat;

public class ErlangChecksRuleDefinitionTest {

  @Test
  public void test() {
    RulesDefinition.Context context = new RulesDefinition.Context();
    ErlangChecksRuleDefinition ruleDefinition = new ErlangChecksRuleDefinition();

    ruleDefinition.define(context);

    RulesDefinition.Repository repository = context.repository(CheckList.REPOSITORY_KEY);

    assertThat(repository.name()).isEqualTo(CheckList.REPOSITORY_NAME);

    List<RulesDefinition.Rule> rules = repository.rules();

    assertThat(rules.size()).isEqualTo(CheckList.getChecks().size());
  }

}
