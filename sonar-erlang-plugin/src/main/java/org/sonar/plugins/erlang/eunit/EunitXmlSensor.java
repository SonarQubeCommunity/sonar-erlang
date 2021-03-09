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

import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.measure.MetricFinder;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.config.Configuration;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.languages.ErlangLanguage;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class EunitXmlSensor implements Sensor {

  private static final Logger LOG = Loggers.get(EunitXmlSensor.class);
  private final MetricFinder metricFinder;

  public EunitXmlSensor(MetricFinder metricFinder) {
    this.metricFinder = metricFinder;
  }

    @Override
    public void describe(SensorDescriptor descriptor) {
        descriptor
                .onlyOnLanguage(ErlangLanguage.KEY)
                .name("Erlang EUnit report Sensor")
                .onlyOnFileType(InputFile.Type.TEST);
    }

    private List<EunitTestsuite> parseEunitXmls(SensorContext context) {
        List<EunitTestsuite> ret = new ArrayList<>();
        FileSystem fileSystem = context.fileSystem();

        FilePredicate eunitXmlPredicate = fileSystem.predicates().matchesPathPattern("**/TEST-*.xml");
        Iterable<File> xmlFiles = fileSystem.files(eunitXmlPredicate);
        for (File file : xmlFiles) {
            XmlMapper mapper = new XmlMapper();
            try {
                ret.add(mapper.readValue(file, EunitTestsuite.class));
            } catch (IOException e) {
                LOG.error("Something went wrong during parsing xml report", e);
            }
            try {
                EunitTestsuites suites = mapper.readValue(file, EunitTestsuites.class);
                ret.addAll(suites.getTestsuites());
            } catch (IOException e) {
                // intentionally left empty
            }
        }
        return ret;
    }

    @Override
    public void execute(SensorContext context) {
        Configuration configuration = context.config();
        FileSystem fileSystem = context.fileSystem();

        configuration.get(ErlangPlugin.EUNIT_FOLDER_KEY).ifPresent(eUnitReportFolder -> {
            File reportsDir = new File(context.fileSystem().baseDir().getPath(), eUnitReportFolder);
            FilePredicate testFilePredicate = fileSystem.predicates().hasLanguage(ErlangLanguage.KEY);

            LOG.debug("Parsing Eunit run results in Surefire format from folder {}", reportsDir);

            List<EunitTestsuite> testReports = parseEunitXmls(context);

            Iterable<InputFile> inputFiles = fileSystem.inputFiles(testFilePredicate);
            for (InputFile file : inputFiles) {
                EunitTestsuite testReport = EunitTestsuite.find(file, testReports);
                if (testReport != null) {
                    saveIntegerMeasure(context, metricFinder, file, CoreMetrics.SKIPPED_TESTS_KEY, testReport.getSkipped());
                    saveIntegerMeasure(context, metricFinder, file, CoreMetrics.TESTS_KEY, testReport.getTests());
                    saveIntegerMeasure(context, metricFinder, file, CoreMetrics.TEST_FAILURES_KEY, testReport.getFailures());
                    saveIntegerMeasure(context, metricFinder, file, CoreMetrics.TEST_ERRORS_KEY, testReport.getErrors());
                    saveLongMeasure(context, metricFinder, file, CoreMetrics.TEST_EXECUTION_TIME_KEY, testReport.getTimeInMs());
                }
            }
        });
    }

  private void saveIntegerMeasure(SensorContext context, MetricFinder metricFinder, InputFile file,
                                  String metric, Integer value) {
    context.newMeasure().forMetric(metricFinder.findByKey(metric)).on(file).withValue(value).save();
  }

  private void saveLongMeasure(SensorContext context, MetricFinder metricFinder, InputFile file,
                               String metric, Long value) {
    context.newMeasure().forMetric(metricFinder.findByKey(metric)).on(file).withValue(value).save();
  }


  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
