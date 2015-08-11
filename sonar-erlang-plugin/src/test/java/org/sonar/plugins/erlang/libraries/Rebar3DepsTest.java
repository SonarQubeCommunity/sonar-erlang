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
package org.sonar.plugins.erlang.libraries;

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.ArgumentCaptor;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.Settings;
import org.sonar.api.design.Dependency;
import org.sonar.api.resources.Library;
import org.sonar.api.resources.Project;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.ProjectUtil;

import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class Rebar3DepsTest {

  private String filename = "rebar3_deps.config";

  private static SensorContext context;

  @Before
  public void setup() throws URISyntaxException {
    context = ProjectUtil.mockContext();
    Project project = new Project("dummy");

    when(context.getResource(any(Library.class))).thenAnswer(new Answer<Library>() {
      @Override
      public Library answer(InvocationOnMock invocation) {
        Object[] args = invocation.getArguments();
        return (Library) args[0];
      }
    });

    Settings settings = new Settings(new PropertyDefinitions(ErlangPlugin.class));
    settings.setProperty(ErlangPlugin.REBAR_CONFIG_FILENAME_KEY, filename);

    ModuleFileSystem fileSystem = ProjectUtil.mockModuleFileSystem(null, null);
    new ErlangLibrarySensor(fileSystem, settings).analyse(project, context);
  }

  @Test
  public void erlangLibrariesTest() throws URISyntaxException {
    ArgumentCaptor<Dependency> argument = ArgumentCaptor.forClass(Dependency.class);
    verify(context, times(3)).saveDependency(argument.capture());
    List<Dependency> capturedDependencies = argument.getAllValues();
    assertThat(((Library) capturedDependencies.get(0).getTo()).getKey(),
      Matchers.equalTo("erlware:episcina"));
    assertThat(((Library) capturedDependencies.get(1).getTo()).getKey(),
      Matchers.equalTo("semiocast:pgsql"));
    assertThat(((Library) capturedDependencies.get(2).getTo()).getKey(),
      Matchers.equalTo("hex:lager"));

    assertThat(((Library) capturedDependencies.get(0).getTo()).getName(),
      Matchers.equalTo("episcina"));
    assertThat(((Library) capturedDependencies.get(1).getTo()).getName(),
      Matchers.equalTo("pgsql"));
    assertThat(((Library) capturedDependencies.get(2).getTo()).getName(),
      Matchers.equalTo("lager"));

    assertThat(((Library) capturedDependencies.get(0).getTo()).getVersion(),
      Matchers.equalTo("master"));
    assertThat(((Library) capturedDependencies.get(1).getTo()).getVersion(),
      Matchers.equalTo("master"));
    assertThat(((Library) capturedDependencies.get(2).getTo()).getVersion(),
      Matchers.equalTo("2.1.1"));
  }
}
