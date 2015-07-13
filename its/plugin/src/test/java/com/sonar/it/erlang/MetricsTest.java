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

public class MetricsTest {

  @ClassRule
  public static Orchestrator orchestrator = ErlangTestSuite.ORCHESTRATOR;

  private static final String PROJECT_KEY = "metrics";
  private static final String FILE_NAME = "erlcount_lib.erl";
  private static Sonar wsClient;

  @BeforeClass
  public static void init() {
    orchestrator.resetData();

    SonarRunner build = ErlangTestSuite.createSonarRunner()
      .setProjectDir(new File("projects/metrics/"))
      .setProjectKey(PROJECT_KEY)
      .setProjectName(PROJECT_KEY)
      .setProjectVersion("1.0")
      .setSourceDirs("src")
      .setProperty("sonar.sourceEncoding", "UTF-8")
      .setProfile("empty-profile");
    orchestrator.executeBuild(build);

    wsClient = orchestrator.getServer().getWsClient();
  }

  @Test
  public void project_level() {
    // Size
    assertThat(getProjectMeasure("ncloc").getIntValue()).isEqualTo(49);
    assertThat(getProjectMeasure("lines").getIntValue()).isEqualTo(69);
    assertThat(getProjectMeasure("files").getIntValue()).isEqualTo(2);
    assertThat(getProjectMeasure("directories").getIntValue()).isEqualTo(1);
    assertThat(getProjectMeasure("functions").getIntValue()).isEqualTo(10);
    assertThat(getProjectMeasure("statements").getIntValue()).isEqualTo(25);
    // Documentation
    assertThat(getProjectMeasure("comment_lines").getIntValue()).isEqualTo(7);
    assertThat(getProjectMeasure("commented_out_code_lines")).isNull();
    assertThat(getProjectMeasure("comment_lines_density").getValue()).isEqualTo(12.5);
    assertThat(getProjectMeasure("public_documented_api_density").getValue()).isEqualTo(16.7);
    // Complexity
    assertThat(getProjectMeasure("complexity").getValue()).isEqualTo(21.0);
    assertThat(getProjectMeasure("function_complexity").getValue()).isEqualTo(2.1);
    assertThat(getProjectMeasure("function_complexity_distribution").getData()).isEqualTo("1=10;2=8;4=2;6=0;8=0;10=0;12=0;20=0;30=0");
    assertThat(getProjectMeasure("file_complexity").getValue()).isEqualTo(10.5);
    assertThat(getProjectMeasure("file_complexity_distribution").getData()).isEqualTo("0=1;5=0;10=1;20=0;30=0;60=0;90=0");
    // Duplication
    assertThat(getProjectMeasure("duplicated_lines").getValue()).isEqualTo(0.0);
    assertThat(getProjectMeasure("duplicated_blocks").getValue()).isEqualTo(0.0);
    assertThat(getProjectMeasure("duplicated_files").getValue()).isEqualTo(0.0);
    assertThat(getProjectMeasure("duplicated_lines_density").getValue()).isEqualTo(0.0);
    // Rules
    assertThat(getProjectMeasure("violations").getValue()).isEqualTo(0.0);
    // Tests
    assertThat(getProjectMeasure("tests")).isNull();
    assertThat(getProjectMeasure("coverage")).isNull();
  }

  @Test
  public void file_level() {
    // Size
    assertThat(getFileMeasure("ncloc").getIntValue()).isEqualTo(41);
    assertThat(getFileMeasure("lines").getIntValue()).isEqualTo(58);
    assertThat(getFileMeasure("functions").getIntValue()).isEqualTo(8);
    assertThat(getFileMeasure("statements").getIntValue()).isEqualTo(23);
    // Documentation
    assertThat(getFileMeasure("comment_lines").getIntValue()).isEqualTo(7);
    assertThat(getFileMeasure("commented_out_code_lines")).isNull();
    assertThat(getFileMeasure("comment_lines_density").getValue()).isEqualTo(14.6);
    // Complexity
    assertThat(getFileMeasure("complexity").getValue()).isEqualTo(19.0);
    assertThat(getFileMeasure("function_complexity").getValue()).isEqualTo(2.4);
    assertThat(getFileMeasure("function_complexity_distribution")).isNull();
    assertThat(getFileMeasure("file_complexity").getValue()).isEqualTo(19.0);

    // Duplication
    assertThat(getFileMeasure("duplicated_lines")).isNull();
    assertThat(getFileMeasure("duplicated_blocks")).isNull();
    assertThat(getFileMeasure("duplicated_files")).isNull();
    assertThat(getFileMeasure("duplicated_lines_density")).isNull();
    // Rules
    assertThat(getFileMeasure("violations")).isNull();
  }

  /* Helper methods */

  private Measure getProjectMeasure(String metricKey) {
    Resource resource = wsClient.find(ResourceQuery.createForMetrics(PROJECT_KEY, metricKey));
    return resource == null ? null : resource.getMeasure(metricKey);
  }

  private Measure getFileMeasure(String metricKey) {
    Resource resource = wsClient.find(ResourceQuery.createForMetrics(keyFor(FILE_NAME), metricKey));
    return resource == null ? null : resource.getMeasure(metricKey);
  }

  private static String keyFor(String s) {
    return PROJECT_KEY + (orchestrator.getConfiguration().getSonarVersion().isGreaterThanOrEquals("4.2") ? ":src/" : ":") + s;
  }
}
