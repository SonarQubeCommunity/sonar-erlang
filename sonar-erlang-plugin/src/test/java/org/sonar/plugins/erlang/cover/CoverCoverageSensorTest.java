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
package org.sonar.plugins.erlang.cover;

import com.google.common.base.Charsets;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.fs.internal.FileMetadata;
import org.sonar.api.batch.sensor.coverage.CoverageType;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.Settings;
import org.sonar.plugins.erlang.ErlangPlugin;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;


public class CoverCoverageSensorTest {

  private Settings settings;
  private SensorContextTester context;
  private File testModuleBasedir = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/");

  @Before
  public void setup() throws URISyntaxException, IOException {
    settings = new Settings(new PropertyDefinitions(ErlangPlugin.class));
    context = SensorContextTester.create(testModuleBasedir);
  }

  private void addFile(SensorContextTester context, String path) {
    DefaultInputFile file = new DefaultInputFile("test", path)
            .setLanguage("erlang")
            .setType(InputFile.Type.MAIN)
            .setModuleBaseDir(testModuleBasedir.toPath());
    file.initMetadata(new FileMetadata().readMetadata(file.file(), Charsets.UTF_8));
    context.fileSystem().add(file);
  }

  @Test
  public void checkCoverSensorWithHtml() throws URISyntaxException {
    settings.setProperty(ErlangPlugin.COVERDATA_FILENAME_KEY, "non_existing.coverdata");
    addFile(context, "src/erlcount_lib.erl");
    context.setSettings(settings);

    new CoverCoverageSensor().execute(context);

    assertThat(context.lineHits("test:src/erlcount_lib.erl", CoverageType.UNIT, 7)).isEqualTo(2);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", CoverageType.UNIT, 10)).isEqualTo(12);
  }

  @Test
  public void checkCoverSensorWithDataFile() throws URISyntaxException {
    settings.setProperty(ErlangPlugin.COVERDATA_FILENAME_KEY, ErlangPlugin.COVERDATA_DEFAULT_FILENAME);
    addFile(context, "src/erlcount_lib.erl");
    context.setSettings(settings);

    new CoverCoverageSensor().execute(context);
    context.lineHits("test:src/erlcount_lib.erl", CoverageType.UNIT, 1);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", CoverageType.UNIT, 7)).isEqualTo(2);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", CoverageType.UNIT, 10)).isEqualTo(12);
  }

}
