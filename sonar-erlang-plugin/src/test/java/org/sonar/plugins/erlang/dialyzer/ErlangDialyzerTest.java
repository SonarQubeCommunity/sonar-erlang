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
package org.sonar.plugins.erlang.dialyzer;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.Settings;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.ActiveRule;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.ProjectUtil;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;

import static org.mockito.Mockito.*;

public class ErlangDialyzerTest {

  private Issuable issuable;

  @Before
  public void setup() throws URISyntaxException, IOException {
    Project project = new Project("dummy");

    Settings settings = new Settings(new PropertyDefinitions(ErlangPlugin.class));
    SensorContext context = ProjectUtil.mockContext();

    RulesProfile rp = mock(RulesProfile.class);
    ActiveRule activeRule = RuleUtil.generateActiveRule("unused_fun", "D019");
    when(rp.getActiveRule(DialyzerRuleDefinition.REPOSITORY_KEY, "D019"))
            .thenReturn(activeRule);
    activeRule = RuleUtil.generateActiveRule("callback_missing", "D041");
    when(rp.getActiveRule(DialyzerRuleDefinition.REPOSITORY_KEY, "D041"))
            .thenReturn(activeRule);

    FileSystem fileSystem = ProjectUtil.createFileSystem(
            "src/test/resources/org/sonar/plugins/erlang/erlcount/",
            Arrays.asList(
                    "src/erlcount_lib.erl",
                    "src/refactorerl_issues.erl"),
            null
    );

    issuable = ProjectUtil.mockIssueable();
    ResourcePerspectives resourcePerspectives = mock(ResourcePerspectives.class);
    when(resourcePerspectives.as(Mockito.eq(Issuable.class), Mockito.any(Resource.class))).thenReturn(issuable);

    new DialyzerReportParser(fileSystem, resourcePerspectives).dialyzer(settings, context, new ErlangRuleManager(
            DialyzerRuleDefinition.DIALYZER_PATH), rp, project);
  }

  @Test
  public void checkDialyzer() {
    ArgumentCaptor<Issue> argument = ArgumentCaptor.forClass(Issue.class);
    verify(issuable, times(3)).addIssue(argument.capture());
  }

}
