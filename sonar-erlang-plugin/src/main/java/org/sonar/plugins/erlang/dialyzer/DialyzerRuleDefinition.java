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
package org.sonar.plugins.erlang.dialyzer;

import com.google.common.base.Charsets;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.api.server.rule.RulesDefinitionXmlLoader;
import org.sonar.plugins.erlang.core.Erlang;

public class DialyzerRuleDefinition implements RulesDefinition {

  public static final String REPOSITORY_NAME = "Dialyzer";
  public static final String REPOSITORY_KEY = "dialyzer";
  public static final String DIALYZER_PATH = "/org/sonar/plugins/erlang/dialyzer/rules.xml";

  private final RulesDefinitionXmlLoader xmlLoader;

  public DialyzerRuleDefinition(RulesDefinitionXmlLoader xmlLoader) {
    this.xmlLoader = xmlLoader;
  }

  @Override
  public void define(Context context) {
    NewRepository repository = context.createRepository(REPOSITORY_KEY, Erlang.KEY).setName(REPOSITORY_NAME);
    xmlLoader.load(repository, getClass().getResourceAsStream(DIALYZER_PATH), Charsets.UTF_8.name());
    repository.done();
  }

}
