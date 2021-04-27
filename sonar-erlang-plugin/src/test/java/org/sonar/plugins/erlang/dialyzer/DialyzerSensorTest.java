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

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.fs.internal.TestInputFileBuilder;
import org.sonar.api.batch.rule.ActiveRules;
import org.sonar.api.batch.rule.internal.ActiveRulesBuilder;
import org.sonar.api.batch.rule.internal.NewActiveRule;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.internal.MapSettings;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.erlang.ErlangPlugin;

import java.io.File;
import java.nio.file.Files;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class DialyzerSensorTest {

  MapSettings settings;
  private final File testModuleBasedir = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/");
  private SensorContextTester context;

  @Before
  public void setup() {
    settings = new MapSettings(new PropertyDefinitions(ErlangPlugin.class));
    context = SensorContextTester.create(testModuleBasedir);

    NewActiveRule rule1 = new NewActiveRule.Builder()
            .setRuleKey(RuleKey.of(DialyzerRuleDefinition.REPOSITORY_KEY, "D019"))
            .setName("unused_fun")
            .build();
    NewActiveRule rule2 = new NewActiveRule.Builder()
            .setRuleKey(RuleKey.of(DialyzerRuleDefinition.REPOSITORY_KEY, "D041"))
            .setName("callback_missing")
            .build();

    ActiveRules rules = new ActiveRulesBuilder()
            .addRule(rule1)
            .addRule(rule2)
            .build();

    context.setActiveRules(rules);

  }

  private void addFile(SensorContextTester context, String path) throws Exception {
    DefaultInputFile dif = new TestInputFileBuilder("test", path)
            .setLanguage("erlang")
            .setType(InputFile.Type.MAIN)
            .setModuleBaseDir(testModuleBasedir.toPath())
            .initMetadata(new String(Files.readAllBytes(testModuleBasedir.toPath().resolve(path))))
            .build();

    context.fileSystem().add(dif);
  }

  @Ignore("Verify that Dialyzer sensor actually works.")
  public void checkDialyzerSensor() throws Exception {
    settings.setProperty(ErlangPlugin.EUNIT_FOLDER_KEY, ErlangPlugin.EUNIT_DEFAULT_FOLDER);
    settings.setProperty(ErlangPlugin.DIALYZER_FILENAME_KEY, ErlangPlugin.DIALYZER_DEFAULT_FILENAME);
    context.setSettings(settings);

    addFile(context, "src/erlcount_lib.erl");
    addFile(context, "src/refactorerl_issues.erl");

    new DialyzerSensor().execute(context);

    assertThat(context.allIssues().size()).isEqualTo(3);
  }

}

