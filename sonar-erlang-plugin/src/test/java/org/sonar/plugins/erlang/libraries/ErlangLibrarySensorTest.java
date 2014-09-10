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
package org.sonar.plugins.erlang.libraries;

import org.apache.commons.configuration.Configuration;
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
import org.sonar.api.config.Settings;
import org.sonar.api.design.Dependency;
import org.sonar.api.resources.Library;
import org.sonar.api.resources.Project;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.ProjectUtil;
import org.sonar.plugins.erlang.core.Erlang;

import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(value = Parameterized.class)
public class ErlangLibrarySensorTest {

  private String filename;

  public ErlangLibrarySensorTest(String filename) {
    this.filename = filename;
  }

  private static SensorContext context;

  @Parameters
  public static Collection<Object[]> data() {
    Object[][] data = new Object[][]{
      {"rebar_comments.config"},
      {"rebar_depsdir.config"}};
    return Arrays.asList(data);
  }

  @Before
  public void setup() throws URISyntaxException {
    context = ProjectUtil.mockContext();
    Project project = new Project("dummy");

    when(context.getResource(any(Library.class))).thenAnswer(new Answer<Library>() {
      public Library answer(InvocationOnMock invocation) {
        Object[] args = invocation.getArguments();
        return (Library) args[0];
      }
    });

    Settings settings = ProjectUtil.createSettings();
    settings.setProperty(ErlangPlugin.REBAR_CONFIG_FILENAME_KEY, filename);

    ModuleFileSystem fileSystem = ProjectUtil.mockModuleFileSystem(null, null);
    new ErlangLibrarySensor(new Erlang(settings), fileSystem).analyse(project, context);
  }

  @Test
  public void erlangLibrariesTest() throws URISyntaxException {
    ArgumentCaptor<Dependency> argument = ArgumentCaptor.forClass(Dependency.class);
    verify(context, times(12)).saveDependency(argument.capture());
    List<Dependency> capturedDependencies = argument.getAllValues();
    assertThat(((Library) capturedDependencies.get(0).getTo()).getKey(),
      Matchers.equalTo("fake:elibs"));
    assertThat(((Library) capturedDependencies.get(1).getTo()).getKey(),
      Matchers.equalTo("kake:estat"));
    assertThat(((Library) capturedDependencies.get(2).getTo()).getKey(),
      Matchers.equalTo("lake:malle"));
    assertThat(((Library) capturedDependencies.get(3).getTo()).getKey(),
      Matchers.equalTo("hola:moke_ads"));
    assertThat(((Library) capturedDependencies.get(4).getTo()).getKey(),
      Matchers.equalTo("malna:eper"));
    assertThat(((Library) capturedDependencies.get(5).getTo()).getKey(),
      Matchers.equalTo("should:meck"));
    assertThat(((Library) capturedDependencies.get(6).getTo()).getKey(),
      Matchers.equalTo("should:meck"));
    assertThat(((Library) capturedDependencies.get(7).getTo()).getKey(),
      Matchers.equalTo("andrewtj:dnssd_erlang"));
    assertThat(((Library) capturedDependencies.get(8).getTo()).getKey(),
      Matchers.equalTo("evanmiller:erlydtl"));
    assertThat(((Library) capturedDependencies.get(9).getTo()).getKey(),
      Matchers.equalTo("basho:lager"));
    assertThat(((Library) capturedDependencies.get(10).getTo()).getKey(),
      Matchers.equalTo("log4erl.googlecode.com:log4erl"));
    assertThat(((Library) capturedDependencies.get(11).getTo()).getKey(),
      Matchers.equalTo("erlang-mbcs.googlecode.com:mbcs"));

    assertThat(((Library) capturedDependencies.get(0).getTo()).getName(),
      Matchers.equalTo("elibs"));
    assertThat(((Library) capturedDependencies.get(1).getTo()).getName(),
      Matchers.equalTo("estat"));
    assertThat(((Library) capturedDependencies.get(2).getTo()).getName(),
      Matchers.equalTo("malle"));
    assertThat(((Library) capturedDependencies.get(3).getTo()).getName(),
      Matchers.equalTo("moke_ads"));
    assertThat(((Library) capturedDependencies.get(4).getTo()).getName(),
      Matchers.equalTo("eper"));
    assertThat(((Library) capturedDependencies.get(5).getTo()).getName(),
      Matchers.equalTo("meck"));
    assertThat(((Library) capturedDependencies.get(6).getTo()).getName(),
      Matchers.equalTo("meck"));
    assertThat(((Library) capturedDependencies.get(7).getTo()).getName(),
      Matchers.equalTo("dnssd"));
    assertThat(((Library) capturedDependencies.get(8).getTo()).getName(),
      Matchers.equalTo("erlydtl"));
    assertThat(((Library) capturedDependencies.get(9).getTo()).getName(),
      Matchers.equalTo("lager"));
    assertThat(((Library) capturedDependencies.get(10).getTo()).getName(),
      Matchers.equalTo("log4erl"));
    assertThat(((Library) capturedDependencies.get(11).getTo()).getName(),
      Matchers.equalTo("mbcs"));

    assertThat(((Library) capturedDependencies.get(0).getTo()).getVersion(),
      Matchers.equalTo("1.1.0"));
    assertThat(((Library) capturedDependencies.get(1).getTo()).getVersion(),
      Matchers.equalTo("0.0.1"));
    assertThat(((Library) capturedDependencies.get(2).getTo()).getVersion(),
      Matchers.equalTo("0.7.2-0"));
    assertThat(((Library) capturedDependencies.get(3).getTo()).getVersion(),
      Matchers.equalTo("19840221-3"));
    assertThat(((Library) capturedDependencies.get(4).getTo()).getVersion(),
      Matchers.equalTo("HEAD"));
    assertThat(((Library) capturedDependencies.get(5).getTo()).getVersion(),
      Matchers.equalTo("0.7.2-0"));
    assertThat(((Library) capturedDependencies.get(6).getTo()).getVersion(),
      Matchers.equalTo("0.7.2-0"));
    assertThat(((Library) capturedDependencies.get(7).getTo()).getVersion(),
      Matchers.equalTo("master"));
    assertThat(((Library) capturedDependencies.get(8).getTo()).getVersion(),
      Matchers.equalTo("dda4db043a"));
    assertThat(((Library) capturedDependencies.get(9).getTo()).getVersion(),
      Matchers.equalTo("master"));
    assertThat(((Library) capturedDependencies.get(10).getTo()).getVersion(),
      Matchers.equalTo("HEAD"));
    assertThat(((Library) capturedDependencies.get(11).getTo()).getVersion(),
      Matchers.equalTo("54"));

  }
}
