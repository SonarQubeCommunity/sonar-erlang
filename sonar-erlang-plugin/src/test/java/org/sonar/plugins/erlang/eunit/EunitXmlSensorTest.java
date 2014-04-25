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
package org.sonar.plugins.erlang.eunit;

import org.apache.commons.configuration.Configuration;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.ProjectUtil;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.net.URISyntaxException;
import java.util.Arrays;

import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.doubleThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class EunitXmlSensorTest {

  private SensorContext context;

  @Before
  public void setup() throws URISyntaxException {
    Project project = new Project("dummy");
    ProjectUtil.addProjectFileSystem(project, "src/test/resources/org/sonar/plugins/erlang/erlcount/test/");

    context = ProjectUtil.mockContext();
    Configuration configuration = ProjectUtil.mockConfiguration();
    when(
        configuration.getString(ErlangPlugin.REBAR_CONFIG_FILENAME_KEY,
            ErlangPlugin.REBAR_DEFAULT_CONFIG_FILENAME)).thenReturn(
        ErlangPlugin.REBAR_DEFAULT_CONFIG_FILENAME);

    ModuleFileSystem fileSystem = ProjectUtil.mockModuleFileSystem(null,
        Arrays.asList(
            new File("src/test/resources/org/sonar/plugins/erlang/erlcount/test/erlcount_eunit.erl"),
            new File("src/test/resources/org/sonar/plugins/erlang/erlcount/test/erlcount_tests.erl")));

    new EunitXmlSensor(new Erlang(configuration), fileSystem).analyse(new Project("dummy"), context);

  }

  @Test
  public void shouldSaveErrorsAndFailuresInXML() throws URISyntaxException {

    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TESTS), eq(7.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.SKIPPED_TESTS), eq(0.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_ERRORS), eq(0.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_FAILURES), eq(1.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_EXECUTION_TIME), doubleThat(Matchers.greaterThan(1.0)));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_SUCCESS_DENSITY), eq(85.71));
  }

}
