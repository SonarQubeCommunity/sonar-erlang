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
package org.sonar.plugins.erlang.dialyzer;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.ActiveRule;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ProjectUtil;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DialyzerSensorTest {

  private Configuration configuration;
  private Erlang erlang;
  private Project project;
  private SensorContext context;
  private Issuable issuable;
  private ResourcePerspectives resourcePerspectives;

  @Before
  public void setup() throws URISyntaxException, IOException {
    configuration = ProjectUtil.mockConfiguration();
    erlang = new Erlang(configuration);
    context = mock(SensorContext.class);

    issuable = ProjectUtil.mockIssueable();
    resourcePerspectives = mock(ResourcePerspectives.class);
    when(resourcePerspectives.as(Mockito.eq(Issuable.class), Mockito.any(Resource.class))).thenReturn(issuable);

    project = new Project("dummy");

    RulesProfile rp = mock(RulesProfile.class);
    ActiveRule activeRule = RuleUtil.generateActiveRule("unused_fun", "D019");
    when(rp.getActiveRule(DialyzerRuleRepository.REPOSITORY_KEY, "D019"))
        .thenReturn(activeRule);
    activeRule = RuleUtil.generateActiveRule("callback_missing", "D041");
    when(rp.getActiveRule(DialyzerRuleRepository.REPOSITORY_KEY, "D041"))
        .thenReturn(activeRule);

    ModuleFileSystem fileSystem = ProjectUtil.mockModuleFileSystem(
        Arrays.asList(
            new File("src/test/resources/org/sonar/plugins/erlang/erlcount/src/erlcount_lib.erl")), null);

    new DialyzerSensor(erlang, rp, fileSystem, resourcePerspectives).analyse(project, context);
  }

  @Test
  public void checkCoverSensor() throws URISyntaxException {
    ArgumentCaptor<Issue> argument = ArgumentCaptor.forClass(Issue.class);
    verify(issuable, times(3)).addIssue(argument.capture());

  }

}
