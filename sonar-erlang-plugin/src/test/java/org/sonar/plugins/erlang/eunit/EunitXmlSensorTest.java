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

import org.sonar.plugins.erlang.ProjectUtil;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.resources.InputFile;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;

public class EunitXmlSensorTest {

  private SensorContext context;
  private Configuration configuration;
  private List<InputFile> srcFiles = new ArrayList<InputFile>();

  @Before
  public void setup() throws URISyntaxException {
    context = ProjectUtil.mockContext();
    configuration = mock(Configuration.class);
    when(
        configuration.getString(ErlangPlugin.EUNIT_FOLDER_KEY,
            ErlangPlugin.EUNIT_DEFAULT_FOLDER)).thenReturn(
        ErlangPlugin.EUNIT_DEFAULT_FOLDER);
    when(
        configuration.getString(ErlangPlugin.REBAR_CONFIG_FILENAME_KEY,
            ErlangPlugin.REBAR_DEFAULT_CONFIG_FILENAME)).thenReturn(
        ErlangPlugin.REBAR_DEFAULT_CONFIG_FILENAME);
    List<InputFile> otherFiles = new ArrayList<InputFile>();
    otherFiles.add(ProjectUtil.getInputFileByPath("/org/sonar/plugins/erlang/erlcount/.eunit/TEST-erlcount_eunit.xml"));
    otherFiles.add(ProjectUtil.getInputFileByPath("/org/sonar/plugins/erlang/erlcount/test/erlcount_eunit.erl"));
    otherFiles.add(ProjectUtil.getInputFileByPath("/org/sonar/plugins/erlang/erlcount/.eunit/TEST-erlcount.xml"));
    otherFiles.add(ProjectUtil.getInputFileByPath("/org/sonar/plugins/erlang/erlcount/test/erlcount_tests.erl"));
    new EunitXmlSensor(new Erlang(configuration)).analyse(ProjectUtil.getProject(srcFiles, otherFiles, configuration), context);

  }

  @Test
  public void shouldSaveErrorsAndFailuresInXML() throws URISyntaxException {

    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TESTS), eq(7.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.SKIPPED_TESTS), eq(0.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_ERRORS), eq(0.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_FAILURES), eq(1.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_EXECUTION_TIME), eq(96.0));
    verify(context, times(2)).saveMeasure((Resource) anyObject(), eq(CoreMetrics.TEST_SUCCESS_DENSITY), eq(85.71));
  }

}
