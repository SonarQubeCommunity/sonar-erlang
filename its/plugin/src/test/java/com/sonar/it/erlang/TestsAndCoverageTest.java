/*
 * SonarSource :: Erlang :: Integration Tests
 * Copyright (C) 2014 SonarSource
 * dev@sonar.codehaus.org
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
package com.sonar.it.erlang;

import com.sonar.orchestrator.Orchestrator;
import com.sonar.orchestrator.build.SonarRunner;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.sonar.wsclient.Sonar;
import org.sonar.wsclient.services.Measure;
import org.sonar.wsclient.services.Resource;
import org.sonar.wsclient.services.ResourceQuery;

import java.io.File;

import static org.fest.assertions.Assertions.assertThat;

public class TestsAndCoverageTest {

  @ClassRule
  public static Orchestrator orchestrator = ErlangTestSuite.ORCHESTRATOR;

  private static final String PROJECT_KEY = "tests_coverage";
  private static Sonar wsClient;

  @BeforeClass
  public static void init() {
    orchestrator.resetData();

    SonarRunner build = ErlangTestSuite.createSonarRunner()
      .setProjectDir(new File("projects/tests_coverage/"))
      .setProjectKey(PROJECT_KEY)
      .setProjectName(PROJECT_KEY)
      .setProjectVersion("1.0")
      .setSourceDirs("src")
      .setTestDirs("test")
      .setProperty("sonar.sourceEncoding", "UTF-8")
      .setProfile("empty-profile");
    orchestrator.executeBuild(build);

    wsClient = orchestrator.getServer().getWsClient();
  }

  @Test
  public void tests() {
    assertThat(getProjectMeasure("tests").getValue()).isEqualTo(14);
    assertThat(getProjectMeasure("test_failures").getValue()).isEqualTo(2);
    assertThat(getProjectMeasure("test_errors").getValue()).isEqualTo(0);
  }

  @Test
  public void coverage() {
    assertThat(getProjectMeasure("lines_to_cover").getValue()).isEqualTo(21);
    assertThat(getProjectMeasure("uncovered_lines").getValue()).isEqualTo(2);
  }

  /* Helper methods */

  private Measure getProjectMeasure(String metricKey) {
    Resource resource = wsClient.find(ResourceQuery.createForMetrics(PROJECT_KEY, metricKey));
    return resource == null ? null : resource.getMeasure(metricKey);
  }

}
