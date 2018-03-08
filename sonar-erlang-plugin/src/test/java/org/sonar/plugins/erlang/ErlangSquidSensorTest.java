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
package org.sonar.plugins.erlang;

import com.google.common.base.Charsets;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;

import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.fs.internal.FileMetadata;
import org.sonar.api.batch.fs.internal.TestInputFileBuilder;
import org.sonar.api.batch.measure.MetricFinder;
import org.sonar.api.batch.rule.ActiveRules;
import org.sonar.api.batch.rule.CheckFactory;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.plugins.erlang.core.Erlang;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.fest.assertions.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ErlangSquidSensorTest {

  private SensorContextTester context;
  private File testModuleBasedir = new File("src/test/resources/");
  private MetricFinder metricFinder;
  private ErlangSquidSensor sensor;

  @Before
  public void setup() throws URISyntaxException, IOException {
    context = SensorContextTester.create(testModuleBasedir.getAbsoluteFile());
    metricFinder = mock(MetricFinder.class);
    when(metricFinder.<Integer>findByKey(CoreMetrics.FILES_KEY)).thenReturn(CoreMetrics.FILES);
    when(metricFinder.<Integer>findByKey(CoreMetrics.LINES_KEY)).thenReturn(CoreMetrics.LINES);
    when(metricFinder.<Integer>findByKey(CoreMetrics.NCLOC_KEY)).thenReturn(CoreMetrics.NCLOC);
    when(metricFinder.<Integer>findByKey(CoreMetrics.FUNCTIONS_KEY)).thenReturn(CoreMetrics.FUNCTIONS);
    when(metricFinder.<Integer>findByKey(CoreMetrics.STATEMENTS_KEY)).thenReturn(CoreMetrics.STATEMENTS);
    when(metricFinder.<Integer>findByKey(CoreMetrics.COMPLEXITY_KEY)).thenReturn(CoreMetrics.COMPLEXITY);
    when(metricFinder.<Integer>findByKey(CoreMetrics.COMMENT_LINES_KEY)).thenReturn(CoreMetrics.COMMENT_LINES);
    when(metricFinder.<Integer>findByKey(CoreMetrics.PUBLIC_API_KEY)).thenReturn(CoreMetrics.PUBLIC_API);
    when(metricFinder.<Integer>findByKey(CoreMetrics.PUBLIC_UNDOCUMENTED_API_KEY))
            .thenReturn(CoreMetrics.PUBLIC_UNDOCUMENTED_API);
    when(metricFinder.<String>findByKey(CoreMetrics.FUNCTION_COMPLEXITY_DISTRIBUTION_KEY))
            .thenReturn(CoreMetrics.FUNCTION_COMPLEXITY_DISTRIBUTION);
    when(metricFinder.<String>findByKey(CoreMetrics.FILE_COMPLEXITY_DISTRIBUTION_KEY))
            .thenReturn(CoreMetrics.FUNCTION_COMPLEXITY_DISTRIBUTION);

    sensor = new ErlangSquidSensor(new CheckFactory(mock(ActiveRules.class)), metricFinder);
  }

  private void addFile(SensorContextTester context, String path) throws IOException {
    DefaultInputFile file = new TestInputFileBuilder("test", path)
                                .setLanguage(Erlang.KEY).setType(InputFile.Type.MAIN)
                                .setModuleBaseDir(context.fileSystem().baseDirPath())
                                .setCharset(UTF_8)
                                .initMetadata(new String(Files.readAllBytes(testModuleBasedir.toPath().resolve(path)), UTF_8))
                                .build();

    /*DefaultInputFile file = new DefaultInputFile("test", path)
            .setLanguage("erlang")
            .setType(InputFile.Type.MAIN)
            .setModuleBaseDir(testModuleBasedir.toPath());

    file.initMetadata(new FileMetadata().readMetadata(file.file(), Charsets.UTF_8));*/
    context.fileSystem().add(file);

  }

  //@Test
  public void analyze_person_erl() throws Exception {
    addFile(context, "cpd/person.erl");
    sensor.execute(context);

    assertThat(context.measure("test:cpd/person.erl", CoreMetrics.FILES_KEY).value()).isEqualTo(1);
    assertThat(context.measure("test:cpd/person.erl", CoreMetrics.LINES_KEY).value()).isEqualTo(19);
    assertThat(context.measure("test:cpd/person.erl", CoreMetrics.NCLOC_KEY).value()).isEqualTo(14);
    assertThat(context.measure("test:cpd/person.erl", CoreMetrics.FUNCTIONS_KEY).value()).isEqualTo(2);
    assertThat(context.measure("test:cpd/person.erl", CoreMetrics.STATEMENTS_KEY).value()).isEqualTo(8);
    assertThat(context.measure("test:cpd/person.erl", CoreMetrics.COMPLEXITY_KEY).value()).isEqualTo(6);
    assertThat(context.measure("test:cpd/person.erl", CoreMetrics.COMMENT_LINES_KEY).value()).isEqualTo(1);
  }

  //@Test
  public void analyse_megaco_erl() throws Exception {
    addFile(context, "megaco_ber_bin_encoder.erl");
    sensor.execute(context);

    assertThat(context.measure("test:megaco_ber_bin_encoder.erl", CoreMetrics.FILES_KEY).value())
            .isEqualTo(1);
    assertThat(context.measure("test:megaco_ber_bin_encoder.erl", CoreMetrics.LINES_KEY).value())
            .isEqualTo(717);
    assertThat(context.measure("test:megaco_ber_bin_encoder.erl", CoreMetrics.NCLOC_KEY).value())
            .isEqualTo(371);
    assertThat(context.measure("test:megaco_ber_bin_encoder.erl", CoreMetrics.FUNCTIONS_KEY).value())
            .isEqualTo(10);
    assertThat(context.measure("test:megaco_ber_bin_encoder.erl", CoreMetrics.STATEMENTS_KEY).value())
            .isEqualTo(210);
    assertThat(context.measure("test:megaco_ber_bin_encoder.erl", CoreMetrics.COMPLEXITY_KEY).value())
            .isEqualTo(90);
    assertThat(context.measure("test:megaco_ber_bin_encoder.erl", CoreMetrics.COMMENT_LINES_KEY).value())
            .isEqualTo(261);
  }

}
