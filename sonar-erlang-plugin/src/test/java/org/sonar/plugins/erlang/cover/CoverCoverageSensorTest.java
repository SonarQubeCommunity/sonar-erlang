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
import org.sonar.api.resources.InputFile;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;
import org.sonar.plugins.erlang.dialyzer.ProjectUtil;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class CoverCoverageSensorTest {

  private ErlangFileCoverage cov;
  private Configuration configuration;
  private Erlang erlang;
  private Project project;
  private SensorContext context;

  @Before
  public void setup() throws URISyntaxException, IOException {
    configuration = mock(Configuration.class);
    when(
        configuration.getString(ErlangPlugin.EUNIT_FOLDER_KEY,
            ErlangPlugin.EUNIT_DEFAULT_FOLDER)).thenReturn(
        ErlangPlugin.EUNIT_DEFAULT_FOLDER);
    erlang = new Erlang(configuration);
    context = mock(SensorContext.class);
  }

  @Test
  public void checkCoverSensor() throws URISyntaxException {
    List<InputFile> srcFiles = new ArrayList<InputFile>();
    List<InputFile> otherFiles = new ArrayList<InputFile>();
    srcFiles.add(ProjectUtil
        .getInputFileByPath("/org/sonar/plugins/erlang/erlcount/.eunit/erlcount_lib.erl"));
    otherFiles
        .add(ProjectUtil
            .getInputFileByPath("/org/sonar/plugins/erlang/erlcount/.eunit/erlcount_lib.COVER.html"));
    project = ProjectUtil.getProject(srcFiles, otherFiles, configuration);
    new CoverCoverageSensor(erlang).analyse(project, context);

    verify(context).saveMeasure((Resource) anyObject(), (Measure) anyObject());
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(21.0));
    verify(context).saveMeasure((Resource) anyObject(), (Metric) anyObject(), eq(2.0));
  }

}
