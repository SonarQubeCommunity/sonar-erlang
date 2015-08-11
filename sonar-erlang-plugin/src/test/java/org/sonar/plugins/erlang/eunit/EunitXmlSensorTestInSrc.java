/*
 * SonarQube Erlang Plugin
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

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.Settings;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.ProjectUtil;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.net.URISyntaxException;
import java.util.Arrays;

import static org.mockito.Matchers.*;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class EunitXmlSensorTestInSrc {

  private SensorContext context;

  @Before
  public void setup() throws URISyntaxException {
    context = ProjectUtil.mockContext();
    Settings settings = new Settings(new PropertyDefinitions(ErlangPlugin.class));
    settings.setProperty(ErlangPlugin.EUNIT_FOLDER_KEY, "eunit");

    FileSystem fileSystem = ProjectUtil.createFileSystem(
            "/",
            Arrays.asList(new File("eunit/lager_crash_log.erl")),
            Arrays.asList(new File("org/sonar/plugins/erlang/erlcount/test/erlcount_eunit.erl"))
    );

    new EunitXmlSensor(new Erlang(settings), fileSystem, settings).analyse(new Project("dummy"), context);
  }

  @Test
  public void shouldSaveErrorsAndFailuresInXML() throws URISyntaxException {
    verify(context, times(1)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TESTS), eq(5.0));
    verify(context, times(1)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.SKIPPED_TESTS), eq(0.0));
    verify(context, times(1)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_ERRORS), eq(0.0));
    verify(context, times(1)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_FAILURES), eq(0.0));
    verify(context, times(1)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_EXECUTION_TIME), doubleThat(Matchers.greaterThan(0.566)));
    verify(context, times(1)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_SUCCESS_DENSITY), eq(100.0));
  }

}
