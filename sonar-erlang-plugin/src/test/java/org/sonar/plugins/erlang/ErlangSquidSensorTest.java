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
package org.sonar.plugins.erlang;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.Mockito;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultFileSystem;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.rule.ActiveRules;
import org.sonar.api.batch.rule.CheckFactory;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.FileLinesContext;
import org.sonar.api.measures.FileLinesContextFactory;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.core.Erlang;
import org.sonar.test.TestUtils;

import java.io.File;

import static org.fest.assertions.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ErlangSquidSensorTest {
  private static final File TEST_DIR = new File("src/test/resources/");

  private ErlangSquidSensor sensor;
  private final DefaultFileSystem fileSystem = new DefaultFileSystem(TEST_DIR);

  @Before
  public void setUp() {
    FileLinesContextFactory fileLinesContextFactory = mock(FileLinesContextFactory.class);
    FileLinesContext fileLinesContext = mock(FileLinesContext.class);

    when(fileLinesContextFactory.createFor(Mockito.any(Resource.class))).thenReturn(fileLinesContext);

    sensor = new ErlangSquidSensor(new CheckFactory(mock(ActiveRules.class)), fileSystem, null);
  }

  @Test
  public void should_execute_on_erlang_project() {
    DefaultFileSystem localFS = new DefaultFileSystem(TEST_DIR);
    ErlangSquidSensor localSensor = new ErlangSquidSensor(new CheckFactory(mock(ActiveRules.class)), localFS, null);

    // empty file system
    assertThat(localSensor.shouldExecuteOnProject(null)).isFalse();

    localFS.add(new DefaultInputFile("key", "file.erl").setType(InputFile.Type.MAIN).setLanguage(Erlang.KEY));
    assertThat(localSensor.shouldExecuteOnProject(null)).isTrue();
  }

  @Test
  public void should_analyse() {
    Project project = new Project("key");
    SensorContext context = mock(SensorContext.class);

    fileSystem.add(new DefaultInputFile("key", "cpd/person.erl")
            .setType(InputFile.Type.MAIN)
            .setLanguage(Erlang.KEY));

    sensor.analyse(project, context);

    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.FILES), Matchers.eq(1.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.LINES), Matchers.eq(19.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.NCLOC), Matchers.eq(14.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.FUNCTIONS), Matchers.eq(2.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.STATEMENTS), Matchers.eq(8.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.COMPLEXITY), Matchers.eq(6.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.COMMENT_LINES), Matchers.eq(1.0));
  }

  @Test
  public void analyse() {
    Project project = new Project("key");
    SensorContext context = mock(SensorContext.class);

    fileSystem.add(new DefaultInputFile("key", "megaco_ber_bin_encoder.erl")
            .setType(InputFile.Type.MAIN)
            .setLanguage(Erlang.KEY));

    sensor.analyse(project, context);

    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.FILES), Matchers.eq(1.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.LINES), Matchers.eq(717.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.NCLOC), Matchers.eq(371.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.FUNCTIONS), Matchers.eq(10.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.STATEMENTS), Matchers.eq(210.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.COMPLEXITY), Matchers.eq(90.0));
    verify(context).saveMeasure(Matchers.any(Resource.class), Matchers.eq(CoreMetrics.COMMENT_LINES), Matchers.eq(261.0));
  }

}
