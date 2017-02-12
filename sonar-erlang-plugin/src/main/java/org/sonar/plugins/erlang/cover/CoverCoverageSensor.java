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
package org.sonar.plugins.erlang.cover;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.config.Settings;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.PropertiesBuilder;
import org.sonar.api.resources.Project;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class CoverCoverageSensor implements Sensor {

  private static final Logger LOG = LoggerFactory.getLogger(CoverCoverageSensor.class);

  protected Settings settings;
  private FileSystem fileSystem;
  private final FilePredicate mainFilePredicate;

  public CoverCoverageSensor(FileSystem fileSystem, Settings settings) {
    this.settings = settings;
    this.fileSystem = fileSystem;
    this.mainFilePredicate = fileSystem.predicates().and(
            fileSystem.predicates().hasType(InputFile.Type.MAIN),
            fileSystem.predicates().hasLanguage(Erlang.KEY));
  }

  @Override
  public boolean shouldExecuteOnProject(Project project) {
    return fileSystem.hasFiles(mainFilePredicate);
  }

  @Override
  public void analyse(Project project, SensorContext context) {
    File reportsDir = new File(fileSystem.baseDir(),
            settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY));

    String coverDataFilename = settings.getString(ErlangPlugin.COVERDATA_FILENAME_KEY);

    File coverDataFile = new File(reportsDir, coverDataFilename);

    if (coverDataFile.exists()) {
      parseCoverdataFile(fileSystem, context, coverDataFile, project);
    } else {
      parseCoverHtmlOutput(fileSystem, context, reportsDir, project);
    }
  }

  private void parseCoverdataFile(FileSystem fileSystem, SensorContext context, File coverDataFile, Project project) {
    try {
      List<ErlangFileCoverage> coveredFiles = CoverDataFileParser.parse(coverDataFile);
      analyseCoveredFiles(fileSystem, context, coveredFiles, project);
    } catch (IOException e) {
      LOG.error("Cannot parse coverdata file: " + coverDataFile.getAbsolutePath(), e);
    }
  }

  private void parseCoverHtmlOutput(FileSystem fileSystem, SensorContext context, File reportsDir, Project project) {
    LOG.debug("Parsing coverage results in html format from folder {}", reportsDir);

    GenericExtFilter filter = new GenericExtFilter(".html");
    String[] list = reportsDir.list(filter);

    if (list == null || list.length == 0) {
      LOG.warn("no files end with .html in {}", reportsDir);
      return;
    }
    List<ErlangFileCoverage> coveredFiles = new ArrayList<ErlangFileCoverage>();
    for (String file : list) {
      if (!file.matches(".*\\.COVER.html")) {
        continue;
      }
      coveredFiles.add(analyseHtml(fileSystem, context, file));
    }
    analyseCoveredFiles(fileSystem, context, coveredFiles, project);
  }

  public ErlangFileCoverage analyseHtml(FileSystem fileSystem, SensorContext sensorContext,
                                        String testCoverageFileName) {
    File coverCoverageReportFile = new File(fileSystem.baseDir(),
            getTestReportsFolder() + "/" + testCoverageFileName);
    LCOVParser parser = new LCOVParser();
    return parser.parseFile(coverCoverageReportFile);
  }

  protected void analyseCoveredFiles(FileSystem fileSystem, SensorContext sensorContext,
                                     List<ErlangFileCoverage> coveredFiles, Project project) {

    for (File file : fileSystem.files(mainFilePredicate)) {
      try {
        ErlangFileCoverage fileCoverage = getFileCoverage(file, coveredFiles);

        InputFile inputFile = fileSystem.inputFile(fileSystem.predicates().is(file));
        org.sonar.api.resources.File sonarFile = org.sonar.api.resources.File.create(inputFile.relativePath());

        PropertiesBuilder<Integer, Integer> lineHitsData = new PropertiesBuilder<Integer, Integer>(
                CoreMetrics.COVERAGE_LINE_HITS_DATA);

        if (fileCoverage != null) {
          Map<Integer, Integer> hits = fileCoverage.getLineCoverageData();
          for (Map.Entry<Integer, Integer> entry : hits.entrySet()) {
            lineHitsData.add(entry.getKey(), entry.getValue());
          }

          sensorContext.saveMeasure(sonarFile, lineHitsData.build());
          sensorContext.saveMeasure(sonarFile, CoreMetrics.LINES_TO_COVER,
                  (double) fileCoverage.getLinesToCover());
          sensorContext.saveMeasure(sonarFile, CoreMetrics.UNCOVERED_LINES,
                  (double) fileCoverage.getUncoveredLines());
        } else {
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
        }

      } catch (Exception e) {
        LOG.error("Problem while calculating coverage for " + file.getAbsolutePath(), e);
      }
    }
  }

  protected ErlangFileCoverage getFileCoverage(File input, List<ErlangFileCoverage> coverages) {
    for (ErlangFileCoverage file : coverages) {
      if (file.getFilePath().equals(input.getAbsolutePath())
              || input.getAbsolutePath().endsWith(file.getFilePath())) {
        return file;
      }
    }
    return null;
  }

  protected String getTestReportsFolder() {
    return settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY);
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
