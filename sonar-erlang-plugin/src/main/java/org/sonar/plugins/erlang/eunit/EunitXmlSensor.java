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
package org.sonar.plugins.erlang.eunit;

import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.measure.MetricFinder;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.config.Settings;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class EunitXmlSensor implements Sensor {

  private static final Logger LOG = LoggerFactory.getLogger(EunitXmlSensor.class);
  private final MetricFinder metricFinder;

  public EunitXmlSensor(MetricFinder metricFinder) {
    this.metricFinder = metricFinder;
  }

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor
            .onlyOnLanguage(Erlang.KEY)
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
    }
    return ret;
  }

  @Override
  public void execute(SensorContext context) {
    Settings settings = context.settings();
    FileSystem fileSystem = context.fileSystem();
    File reportsDir = new File(context.fileSystem().baseDir().getPath(),
            settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY));
    FilePredicate testFilePredicate = fileSystem.predicates().and(
            fileSystem.predicates().hasType(InputFile.Type.TEST),
            fileSystem.predicates().hasLanguage(Erlang.KEY));

    LOG.debug("Parsing Eunit run results in Surefile format from folder {}", reportsDir);

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
  }

  private void saveIntegerMeasure(SensorContext context, MetricFinder metricFinder, InputFile file,
                           String metric, Integer value) {
    context.newMeasure().forMetric(metricFinder.findByKey(metric)).on(file).withValue(value).save();
  }

  private void saveLongMeasure(SensorContext context, MetricFinder metricFinder, InputFile file,
                                  String metric, Long value) {
    context.newMeasure().forMetric(metricFinder.findByKey(metric)).on(file).withValue(value).save();
  }

  private void saveDoubleMeasure(SensorContext context, MetricFinder metricFinder, InputFile file,
                               String metric, Double value) {
    context.newMeasure().forMetric(metricFinder.findByKey(metric)).on(file).withValue(value).save();
  }


  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
