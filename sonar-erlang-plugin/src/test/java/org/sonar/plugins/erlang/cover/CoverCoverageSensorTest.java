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

import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.Settings;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.ProjectUtil;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;

import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

public class CoverCoverageSensorTest {

  private Settings settings;
  private Project project;
  private SensorContext context;

  @Before
  public void setup() throws URISyntaxException, IOException {
    settings = new Settings(new PropertyDefinitions(ErlangPlugin.class));
    context = ProjectUtil.mockContext();
    project = new Project("dummy");
  }

  @Test
  public void checkCoverSensor() throws URISyntaxException {
    settings.setProperty(ErlangPlugin.COVERDATA_FILENAME_KEY, "non_existing.coverdata");

    FileSystem fileSystem = ProjectUtil.createFileSystem(
            "src/test/resources/org/sonar/plugins/erlang/erlcount/",
            Arrays.asList("src/erlcount_lib.erl"),
            null
    );

    new CoverCoverageSensor(fileSystem, settings).analyse(project, context);

    verify(context).saveMeasure((Resource) anyObject(), (Measure) anyObject());
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(21.0));
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(2.0));
  }

  @Test
  public void checkCoverSensorWithDataFile() throws URISyntaxException {
    settings.setProperty(ErlangPlugin.COVERDATA_FILENAME_KEY, ErlangPlugin.COVERDATA_DEFAULT_FILENAME);

    FileSystem fileSystem = ProjectUtil.createFileSystem(
            "src/test/resources/org/sonar/plugins/erlang/erlcount/",
            Arrays.asList("erlcount_lib.erl"),
            null
    );

    new CoverCoverageSensor(fileSystem, settings).analyse(project, context);

    verify(context).saveMeasure((Resource) anyObject(), (Measure) anyObject());
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(21.0));
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(2.0));
  }

}
