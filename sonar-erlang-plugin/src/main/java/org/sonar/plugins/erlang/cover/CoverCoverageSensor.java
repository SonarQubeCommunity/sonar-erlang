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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
    File reportsDir = new File(context.fileSystem().baseDir().getPath(),
            configuration.get(ErlangPlugin.EUNIT_FOLDER_KEY).orElse(ErlangPlugin.EUNIT_DEFAULT_FOLDER));

    Optional<String> coverDataFilename = configuration.get(ErlangPlugin.COVERDATA_FILENAME_KEY);

    if (!coverDataFilename.isPresent()) {
      LOG.warn("Missing cover data file name in configuration");
    } else {
      File coverDataFile = new File(reportsDir, coverDataFilename.get());

      if (coverDataFile.exists()) {
        parseCoverdataFile(context.fileSystem(), context, coverDataFile);
      } else {
        parseCoverHtmlOutput(context.fileSystem(), context, reportsDir);
      }
    }
  }

  private void parseCoverdataFile(FileSystem fileSystem, SensorContext context, File coverDataFile) {
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
    String[] list = reportsDir.list(filter);

    if (list == null || list.length == 0) {
      LOG.warn("no files end with .html in {}", reportsDir);
      return;
    }
    List<ErlangFileCoverage> coveredFiles = new ArrayList<>();
    for (String file : list) {
      if (!file.matches(".*\\.COVER.html")) {
        continue;
      }
      String reportsFolder = getTestReportsFolder(context.config());
      coveredFiles.add(analyseHtml(fileSystem, reportsFolder, file));
    }
    analyseCoveredFiles(fileSystem, context, coveredFiles);
  }

  private ErlangFileCoverage analyseHtml(FileSystem fileSystem, String reportsFolder, String testCoverageFileName) {
    File coverCoverageReportFile = new File(fileSystem.baseDir(),
            reportsFolder + "/" + testCoverageFileName);
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

  private String getTestReportsFolder(Configuration configuration) {
    return configuration.get(ErlangPlugin.EUNIT_FOLDER_KEY).orElse(ErlangPlugin.EUNIT_DEFAULT_FOLDER);
  }

  private NewCoverage getNewCoverageForFile(InputFile inputFile, SensorContext sensorContext, ErlangFileCoverage erlangFileCoverage) {
    NewCoverage coverage = sensorContext.newCoverage().onFile(inputFile);
    Map<Integer, Integer> hits = erlangFileCoverage.getLineCoverageData();
    for (Map.Entry<Integer, Integer> entry : hits.entrySet()) {
      coverage.lineHits(entry.getKey(), entry.getValue());
    }

    return coverage;
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
