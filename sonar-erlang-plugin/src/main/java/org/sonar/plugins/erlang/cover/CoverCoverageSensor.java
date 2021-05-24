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
package org.sonar.plugins.erlang.cover;

import org.sonar.api.batch.fs.FilePredicates;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.batch.sensor.coverage.NewCoverage;
import org.sonar.api.config.Configuration;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.languages.ErlangLanguage;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class CoverCoverageSensor implements Sensor {

  private static final Logger LOG = Loggers.get(CoverCoverageSensor.class);

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor
        .onlyOnLanguage(ErlangLanguage.KEY)
        .name("Erlang Analyser Sensor")
        .onlyOnFileType(InputFile.Type.MAIN);
  }

  @Override
  public void execute(SensorContext context) {
    Configuration configuration = context.config();

    String eunitCoverDataFileName = getEunitCoverageFileName(configuration);
    String commonTestCoverDataFileName = getCommonTestCoverageFileName(configuration);

    File eunitReportsDir = new File(context.fileSystem().baseDir().getPath(), getEunitTestReportsFolder(configuration));
    File eunitCoverDataFile = new File(eunitReportsDir, eunitCoverDataFileName);
    File commonTestCoverDataFile = new File(context.fileSystem().baseDir().getPath(), commonTestCoverDataFileName);

    // We run the same cover parsing for CommonTest, sensor will save max values for coverage metrics
    if (commonTestCoverDataFile.exists()) {
      LOG.debug("Found Common Test coverage report file at {}", commonTestCoverDataFile.getPath());
      parseCoverdataFile(context.fileSystem(), context, commonTestCoverDataFile);
    }

    if (eunitCoverDataFile.exists()) {
      LOG.debug("Found Eunit coverage report file at {}", eunitCoverDataFile.getPath());
      parseCoverdataFile(context.fileSystem(), context, eunitCoverDataFile);
    } else {
      parseCoverHtmlOutput(context.fileSystem(), context, eunitReportsDir);
    }

  }

  private void parseCoverdataFile(FileSystem fileSystem, SensorContext context, File coverDataFile) {
    LOG.debug("Trying to parse coverage data file: {}", coverDataFile.getName());
    try {
      List<ErlangFileCoverage> coveredFiles = CoverDataFileParser.parse(coverDataFile);
      analyseCoveredFiles(fileSystem, context, coveredFiles);
    } catch (IOException e) {
      LOG.error("Cannot parse cover data file: {}", coverDataFile.getAbsolutePath(), e);
    }
  }

  private void parseCoverHtmlOutput(FileSystem fileSystem, SensorContext context, File reportsDir) {
    LOG.debug("Parsing coverage results in html format from folder {}", reportsDir);

    GenericExtFilter filter = new GenericExtFilter(".html");
    String[] fileList = reportsDir.list(filter);

    if (fileList == null || fileList.length == 0) {
      LOG.warn("no files end with .html in {}", reportsDir);
      return;
    }

    String reportsFolder = getEunitTestReportsFolder(context.config());

    List<ErlangFileCoverage> coveredFiles = Arrays.stream(fileList)
        .filter(fileName -> fileName.matches(".*\\.COVER.html"))
        .map(fileName -> analyseHtml(fileSystem, reportsFolder, fileName))
        .collect(Collectors.toList());

    analyseCoveredFiles(fileSystem, context, coveredFiles);
  }

  private ErlangFileCoverage analyseHtml(FileSystem fileSystem, String reportsFolder, String testCoverageFileName) {
    Path reportsPath = Paths.get(fileSystem.baseDir().toString(), reportsFolder);
    File coverCoverageReportFile = new File(reportsPath.toFile(), testCoverageFileName);
    LCOVParser parser = new LCOVParser();
    return parser.parseFile(coverCoverageReportFile);
  }

  private void analyseCoveredFiles(FileSystem fileSystem, SensorContext sensorContext,
                                   List<ErlangFileCoverage> coveredFiles) {

    FilePredicates p = fileSystem.predicates();
    Iterable<InputFile> inputFiles = fileSystem.inputFiles(p.and(p.hasType(InputFile.Type.MAIN), p.hasLanguage(ErlangLanguage.KEY)));
    for (InputFile file : inputFiles) {
      try {
        ErlangFileCoverage fileCoverage = getFileCoverage(file, coveredFiles);

        if (fileCoverage != null) {
          NewCoverage coverage = getNewCoverageForFile(file, sensorContext, fileCoverage);
          coverage.save();
        }

      } catch (Exception e) {
        LOG.error("Problem while calculating coverage for {}", file.filename(), e);
      }
    }
  }

  private ErlangFileCoverage getFileCoverage(InputFile input, List<ErlangFileCoverage> coverages) {
    return coverages
        .stream()
        .filter(erlangFileCoverage -> erlangFileCoverage.getFilePath().endsWith(input.filename()))
        .collect(Collectors.toList())
        .get(0);
  }

  private String getEunitTestReportsFolder(Configuration configuration) {
    return configuration.get(ErlangPlugin.EUNIT_FOLDER_KEY).orElse(ErlangPlugin.EUNIT_DEFAULT_FOLDER);
  }

  private String getEunitCoverageFileName(Configuration configuration) {
    return configuration.get(ErlangPlugin.EUNIT_COVERDATA_FILENAME_KEY).orElse(ErlangPlugin.EUNIT_COVERDATA_DEFAULT_FILENAME);
  }

  private String getCommonTestCoverageFileName(Configuration configuration) {
    return configuration.get(ErlangPlugin.CT_COVERDATA_FILENAME_KEY).orElse(ErlangPlugin.CT_COVERDATA_DEFAULT_FILENAME);
  }

  private NewCoverage getNewCoverageForFile(InputFile inputFile, SensorContext sensorContext, ErlangFileCoverage erlangFileCoverage) {
    NewCoverage coverage = sensorContext.newCoverage().onFile(inputFile);
    Map<Integer, Integer> hits = erlangFileCoverage.getLineCoverageData();
    hits
        .entrySet()
        .stream()
        .forEach(line -> coverage.lineHits(line.getKey(), line.getValue()));

    return coverage;
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
