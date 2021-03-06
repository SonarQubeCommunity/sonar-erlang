/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2018 Tamas Kende; Denes Hegedus (Cursor Insight Ltd.)
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
package org.sonar.plugins.erlang.cover;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.fs.FilePredicates;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.coverage.CoverageType;
import org.sonar.api.batch.sensor.coverage.NewCoverage;
import org.sonar.api.config.Settings;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CoverCoverageSensor implements Sensor {

  private static final Logger LOG = LoggerFactory.getLogger(CoverCoverageSensor.class);

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor
            .onlyOnLanguage(Erlang.KEY)
            .name("Erlang Analyser Sensor")
            .onlyOnFileType(InputFile.Type.MAIN);
  }

  @Override
  public void execute(SensorContext context) {
    Settings settings = context.settings();
    File reportsDir = new File(context.fileSystem().baseDir().getPath(),
            settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY));

    String coverDataFilename = settings.getString(ErlangPlugin.COVERDATA_FILENAME_KEY);

    File coverDataFile = new File(reportsDir, coverDataFilename);

    if (coverDataFile.exists()) {
      parseCoverdataFile(context.fileSystem(), context, coverDataFile);
    } else {
      parseCoverHtmlOutput(context.fileSystem(), context, reportsDir);
    }
  }

  private void parseCoverdataFile(FileSystem fileSystem, SensorContext context, File coverDataFile) {
    try {
      List<ErlangFileCoverage> coveredFiles = CoverDataFileParser.parse(coverDataFile);
      analyseCoveredFiles(fileSystem, context, coveredFiles);
    } catch (IOException e) {
      LOG.error("Cannot parse coverdata file: " + coverDataFile.getAbsolutePath(), e);
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
      String reportsFolder = getTestReportsFolder(context.settings());
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
    Iterable<InputFile> inputFiles = fileSystem.inputFiles(p.and(p.hasType(InputFile.Type.MAIN), p.hasLanguage(Erlang.KEY)));
    for (InputFile file : inputFiles) {
      try {
        ErlangFileCoverage fileCoverage = getFileCoverage(file, coveredFiles);

        if (fileCoverage != null) {
          NewCoverage coverage = sensorContext.newCoverage()
                  .ofType(CoverageType.UNIT)
                  .onFile(file);
          Map<Integer, Integer> hits = fileCoverage.getLineCoverageData();
          for (Map.Entry<Integer, Integer> entry : hits.entrySet()) {
            coverage.lineHits(entry.getKey(), entry.getValue());
          }
          coverage.save();
        } /*else {

          // colour all lines as not executed
          for (int x = 1; x < sensorContext.getMeasure(sonarFile, CoreMetrics.LINES)
                  .getIntValue(); x++) {
            lineHitsData.add(x, 0);
          }

          // use non comment lines of code for coverage calculation
          Measure ncloc = sensorContext.getMeasure(sonarFile, CoreMetrics.NCLOC);
          sensorContext.saveMeasure(sonarFile, lineHitsData.build());
          sensorContext.saveMeasure(sonarFile, CoreMetrics.LINES_TO_COVER, ncloc
                  .getValue());
          sensorContext.saveMeasure(sonarFile, CoreMetrics.UNCOVERED_LINES, ncloc
                  .getValue());

        }*/

      } catch (Exception e) {
        LOG.error("Problem while calculating coverage for " + file.absolutePath(), e);
      }
    }
  }

  private ErlangFileCoverage getFileCoverage(InputFile input, List<ErlangFileCoverage> coverages) {
    for (ErlangFileCoverage file : coverages) {
      if (file.getFilePath().equals(input.absolutePath())
              || input.absolutePath().endsWith(file.getFilePath())) {
        return file;
      }
    }
    return null;
  }

  private String getTestReportsFolder(Settings settings) {
    return settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY);
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
