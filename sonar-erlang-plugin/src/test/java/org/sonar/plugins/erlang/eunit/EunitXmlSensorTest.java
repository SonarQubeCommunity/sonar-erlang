/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 * Copyright © 2021 Daniils Petrovs <dpetrovs@evolution.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.sonar.plugins.erlang.eunit;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultFileSystem;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.fs.internal.TestInputFileBuilder;
import org.sonar.api.batch.measure.MetricFinder;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.api.config.internal.MapSettings;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.plugins.erlang.ErlangPlugin;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static org.fest.assertions.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class EunitXmlSensorTest {

    private EunitXmlSensor sensor;
    private SensorContextTester context;

    @Rule
    public TemporaryFolder temp = new TemporaryFolder();

    private File baseDir;
    private MetricFinder metricFinder;

    @Before
    public void prepare() throws IOException {
        baseDir = temp.newFolder();
        metricFinder = mock(MetricFinder.class);
        when(metricFinder.<Integer>findByKey(CoreMetrics.TESTS_KEY)).thenReturn(CoreMetrics.TESTS);
        when(metricFinder.<Integer>findByKey(CoreMetrics.SKIPPED_TESTS_KEY)).thenReturn(CoreMetrics.SKIPPED_TESTS);
        when(metricFinder.<Integer>findByKey(CoreMetrics.TEST_ERRORS_KEY)).thenReturn(CoreMetrics.TEST_ERRORS);
        when(metricFinder.<Integer>findByKey(CoreMetrics.TEST_FAILURES_KEY)).thenReturn(CoreMetrics.TEST_FAILURES);
        when(metricFinder.<Long>findByKey(CoreMetrics.TEST_EXECUTION_TIME_KEY)).thenReturn(CoreMetrics.TEST_EXECUTION_TIME);
        sensor = new EunitXmlSensor(metricFinder);
        context = SensorContextTester.create(baseDir);
    }

    @Test
    public void test1() {
        File baseDir = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/");
        addFile(context.fileSystem(), baseDir, "test/erlcount_tests.erl", "erlang", InputFile.Type.TEST);
        addFile(context.fileSystem(), baseDir, ".eunit/TEST-erlcount_tests.xml", "xml", InputFile.Type.TEST);

        sensor.execute(context);

        assertThat( context.measure("project:test/erlcount_tests.erl", CoreMetrics.TESTS_KEY).value()).isEqualTo(7);
        assertThat(context.measure("project:test/erlcount_tests.erl", CoreMetrics.SKIPPED_TESTS_KEY).value()).isEqualTo(0);
        assertThat(context.measure("project:test/erlcount_tests.erl", CoreMetrics.TEST_ERRORS_KEY).value()).isEqualTo(0);
        assertThat(context.measure("project:test/erlcount_tests.erl", CoreMetrics.TEST_FAILURES_KEY).value()).isEqualTo(1);
        assertThat(context.measure("project:test/erlcount_tests.erl", CoreMetrics.TEST_EXECUTION_TIME_KEY).value()).isEqualTo(133L);
    }

    @Test
    public void test2() {
        MapSettings settings = new MapSettings().setProperty(ErlangPlugin.EUNIT_FOLDER_KEY, "eunit");
        context.setSettings(settings);

        File baseDir = new File("src/test/resources");
        addFile(context.fileSystem(), baseDir, "eunit/lager_crash_log.erl", "erlang", InputFile.Type.TEST);
        addFile(context.fileSystem(), baseDir, "eunit/TEST-lager_crash_log.xml", "xml", InputFile.Type.TEST);

        sensor.execute(context);

        assertThat( context.measure("project:eunit/lager_crash_log.erl", CoreMetrics.TESTS_KEY).value()).isEqualTo(5);
        assertThat(context.measure("project:eunit/lager_crash_log.erl", CoreMetrics.SKIPPED_TESTS_KEY).value()).isEqualTo(0);
        assertThat(context.measure("project:eunit/lager_crash_log.erl", CoreMetrics.TEST_ERRORS_KEY).value()).isEqualTo(0);
        assertThat(context.measure("project:eunit/lager_crash_log.erl", CoreMetrics.TEST_FAILURES_KEY).value()).isEqualTo(0);
        assertThat(context.measure("project:eunit/lager_crash_log.erl", CoreMetrics.TEST_EXECUTION_TIME_KEY).value()).isEqualTo(567L);
    }

    private static void addFile(DefaultFileSystem fs, File dir, String file, String lang, InputFile.Type type) {
        byte[] content;
        try {
            content = Files.readAllBytes(dir.toPath().resolve(file));
        } catch (IOException e) {
            content = new byte[0];
        }
        DefaultInputFile dif = new TestInputFileBuilder("project", file)
                .setLanguage(lang)
                .setType(type)
                .setModuleBaseDir(dir.toPath())
                .setContents(new String(content))
                .build();
        fs.add(dif);
    }
}