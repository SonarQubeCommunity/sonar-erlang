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
package org.sonar.plugins.erlang.cover;

import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.fs.internal.TestInputFileBuilder;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.batch.sensor.internal.DefaultSensorDescriptor;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.internal.MapSettings;
import org.sonar.plugins.erlang.ErlangPlugin;

import java.io.File;
import java.nio.file.Files;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class CoverCoverageSensorTest {
  private MapSettings settings;
  private SensorContextTester context;
  private final File testModuleBasedir = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/");

  @Before
  public void setup() {
    settings = new MapSettings(new PropertyDefinitions(ErlangPlugin.class));
    context = SensorContextTester.create(testModuleBasedir);
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

  @Test
  public void checkCoverSensorWithHtmlWithCommonTestDefaults() throws Exception {
    settings.setProperty(ErlangPlugin.EUNIT_COVERDATA_FILENAME_KEY, "non_existing.coverdata");
    addFile(context, "src/erlcount_lib.erl");
    context.setSettings(settings);

    new CoverCoverageSensor().execute(context);

    assertThat(context.lineHits("test:src/erlcount_lib.erl", 7)).isEqualTo(3);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", 10)).isEqualTo(18);
  }

  @Test
  public void checkCoverSensorWithDataFileWithCommonTestDefaults() throws Exception {
    settings.setProperty(ErlangPlugin.EUNIT_COVERDATA_FILENAME_KEY, ErlangPlugin.EUNIT_COVERDATA_DEFAULT_FILENAME);
    addFile(context, "src/erlcount_lib.erl");
    context.setSettings(settings);

    new CoverCoverageSensor().execute(context);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", 7)).isEqualTo(3);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", 10)).isEqualTo(18);
  }

  @Test
  public void checkCoverSensorWithDataFileWithCommonTest() throws Exception {
    settings.setProperty(ErlangPlugin.EUNIT_COVERDATA_FILENAME_KEY, ErlangPlugin.EUNIT_COVERDATA_DEFAULT_FILENAME);
    settings.setProperty(ErlangPlugin.CT_COVERDATA_FILENAME_KEY, ErlangPlugin.CT_COVERDATA_DEFAULT_FILENAME);
    addFile(context, "src/erlcount_lib.erl");
    context.setSettings(settings);

    new CoverCoverageSensor().execute(context);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", 7)).isEqualTo(3);
    assertThat(context.lineHits("test:src/erlcount_lib.erl", 10)).isEqualTo(18);
  }

  @Test
  public void checkCoverSensorWithDataFileCommonTestOnlyCustomDataFile() throws Exception {
    // Make sure no Eunit file parsing happens
    settings.setProperty(ErlangPlugin.EUNIT_FOLDER_KEY, "nonexistent.eunit.folder");
    settings.setProperty(ErlangPlugin.EUNIT_COVERDATA_FILENAME_KEY, "nonexistent.eunit.coverdata.heh");
    settings.setProperty(ErlangPlugin.CT_COVERDATA_FILENAME_KEY, ErlangPlugin.CT_COVERDATA_DEFAULT_FILENAME);
    addFile(context, "src/erlcount_lib.erl");
    context.setSettings(settings);

    new CoverCoverageSensor().execute(context);

    assertThat(context.lineHits("test:src/erlcount_lib.erl", 10)).isEqualTo(6);
  }

  @Test
  public void testDescribe() {
    CoverCoverageSensor coverCoverageSensor = new CoverCoverageSensor();
    SensorDescriptor descriptor = new DefaultSensorDescriptor();
    coverCoverageSensor.describe(descriptor);
    assertThat(descriptor).isNotNull();
  }

}
