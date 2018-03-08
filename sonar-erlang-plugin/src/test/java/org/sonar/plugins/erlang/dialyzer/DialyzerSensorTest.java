/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;

import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.fs.internal.FileMetadata;
import org.sonar.api.batch.fs.internal.TestInputFileBuilder;
import org.sonar.api.batch.rule.ActiveRules;
import org.sonar.api.batch.rule.internal.ActiveRulesBuilder;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.Settings;
import org.sonar.api.config.internal.MapSettings;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.erlang.ErlangPlugin;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class DialyzerSensorTest {

  private File testModuleBasedir = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/");
  private Settings settings;
  private SensorContextTester context;

  @Before
  public void setup() throws URISyntaxException, IOException {
    settings = new MapSettings(new PropertyDefinitions(ErlangPlugin.class));
    context = SensorContextTester.create(testModuleBasedir);
    ActiveRules rules = (new ActiveRulesBuilder())
            .create(RuleKey.of(DialyzerRuleDefinition.REPOSITORY_KEY, "D019"))
            .setName("unused_fun")
            .activate()
            .create(RuleKey.of(DialyzerRuleDefinition.REPOSITORY_KEY, "D041"))
            .setName("callback_missing")
            .activate()
            .build();
    context.setActiveRules(rules);
  }

  private void addFile(SensorContextTester context, String path) throws Exception{
    /*DefaultInputFile file = new DefaultInputFile("test", path)
            .setLanguage("erlang")
            .setType(InputFile.Type.MAIN)
            .setModuleBaseDir(testModuleBasedir.toPath());
    file.initMetadata(new FileMetadata().readMetadata(file.file(), Charsets.UTF_8));*/

    DefaultInputFile dif = new TestInputFileBuilder("test", path)
            .setLanguage("erlang").setType(InputFile.Type.MAIN)
            .setModuleBaseDir(testModuleBasedir.toPath())
            .initMetadata(new String(Files.readAllBytes(testModuleBasedir.toPath().resolve(path))))
            .build();

    context.fileSystem().add(dif);
  }

  @Test
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
