/*
 * Sonar Erlang Plugin
 * Copyright (C) 2012 Tamas Kende
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
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.sonar.plugins.erlang.cover;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.ProjectUtil;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;

import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class CoverCoverageSensorTest {

  private Configuration configuration;
  private Erlang erlang;
  private Project project;
  private SensorContext context;

  @Before
  public void setup() throws URISyntaxException, IOException {
    configuration = ProjectUtil.mockConfiguration();
    erlang = new Erlang(configuration);
    context = ProjectUtil.mockContext();
    project = new Project("dummy");
    ProjectUtil.addProjectFileSystem(project, "src/test/resources/org/sonar/plugins/erlang/erlcount/src/");
  }

  @Test
  public void checkCoverSensor() throws URISyntaxException {
    when(configuration.getString(ErlangPlugin.COVERDATA_FILENAME_KEY, ErlangPlugin.COVERDATA_DEFAULT_FILENAME))
      .thenReturn("non_existing.coverdata");

    ModuleFileSystem fileSystem = ProjectUtil.mockModuleFileSystem(
      Arrays.asList(
        new File("src/test/resources/org/sonar/plugins/erlang/erlcount/src/erlcount_lib.erl")), null);

    new CoverCoverageSensor(erlang, fileSystem).analyse(project, context);

    verify(context).saveMeasure((Resource) anyObject(), (Measure) anyObject());
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(21.0));
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(2.0));
  }

  @Test
  public void checkCoverSensorWithDataFile() throws URISyntaxException {
    when(configuration.getString(ErlangPlugin.COVERDATA_FILENAME_KEY, ErlangPlugin.COVERDATA_DEFAULT_FILENAME))
      .thenReturn(ErlangPlugin.COVERDATA_DEFAULT_FILENAME);

    ModuleFileSystem fileSystem = ProjectUtil.mockModuleFileSystem(
      Arrays.asList(
        new File("src/test/resources/org/sonar/plugins/erlang/erlcount/src/erlcount_lib.erl")), null);

    new CoverCoverageSensor(erlang, fileSystem).analyse(project, context);

    verify(context).saveMeasure((Resource) anyObject(), (Measure) anyObject());
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(21.0));
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(2.0));
  }

}
