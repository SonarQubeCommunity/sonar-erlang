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
package org.sonar.plugins.erlang.elvis;

import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.api.server.rule.RulesDefinitionXmlLoader;
import org.sonar.plugins.erlang.languages.ErlangLanguage;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class ElvisRuleDefinition implements RulesDefinition {

  public static final String REPOSITORY_NAME = "Elvis";
  public static final String REPOSITORY_KEY = "elvis";
  public static final String ELVIS_PATH = "/org/sonar/plugins/erlang/elvis/rules.xml";

  @Override
  public void define(Context context) {
    NewRepository repository = context.createRepository(REPOSITORY_KEY, ErlangLanguage.KEY).setName(REPOSITORY_NAME);
    InputStream rulesXml = this.getClass().getResourceAsStream(ELVIS_PATH);
    if (rulesXml != null) {
      RulesDefinitionXmlLoader rulesLoader = new RulesDefinitionXmlLoader();
      rulesLoader.load(repository, rulesXml, StandardCharsets.UTF_8);
    }
    repository.done();
  }

}
