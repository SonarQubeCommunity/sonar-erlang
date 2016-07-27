/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2016 Tamas Kende
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
package org.sonar.plugins.erlang.checks;

import org.junit.Test;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.erlang.checks.CheckList;

import java.util.List;

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
