/*
 * Sonar Erlang Plugin
 * Copyright (C) 2012 Tamas Kende
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
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.sonar.plugins.erlang.cover;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.PropertiesBuilder;
import org.sonar.api.resources.Project;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class CoverCoverageSensor implements Sensor {

  protected Erlang erlang;
  private ModuleFileSystem moduleFileSystem;

  public CoverCoverageSensor(Erlang erlang, ModuleFileSystem moduleFileSystem) {
    this.erlang = erlang;
    this.moduleFileSystem = moduleFileSystem;
  }

  private static final Logger LOG = LoggerFactory.getLogger(CoverCoverageSensor.class);

  public boolean shouldExecuteOnProject(Project project) {
    return !moduleFileSystem.files(Erlang.sourceQuery).isEmpty();
  }

  public void analyse(Project project, SensorContext context) {
    File reportsDir = new File(moduleFileSystem.baseDir(), erlang.getConfiguration()
        .getString(ErlangPlugin.EUNIT_FOLDER_KEY, ErlangPlugin.EUNIT_DEFAULT_FOLDER));

    String coverDataFilename = erlang.getConfiguration().getString(ErlangPlugin.COVERDATA_FILENAME_KEY, ErlangPlugin.COVERDATA_DEFAULT_FILENAME);

    File coverDataFile = new File(reportsDir, coverDataFilename);

    if(coverDataFile.exists()){
      parseCoverdataFile(moduleFileSystem, context, coverDataFile, project);
    } else{
      parseCoverHtmlOutput(moduleFileSystem, context, reportsDir, project);
    }
  }

  private void parseCoverdataFile(ModuleFileSystem moduleFileSystem, SensorContext context, File coverDataFile, Project project) {
    try {
      List<ErlangFileCoverage> coveredFiles = CoverDataFileParser.parse(coverDataFile);
      analyseCoveredFiles(moduleFileSystem, context, coveredFiles, project);
    } catch (IOException e) {
      LOG.error("Cannot parse coverdata file: "+coverDataFile.getAbsolutePath());
    }
  }

  private void parseCoverHtmlOutput(ModuleFileSystem moduleFileSystem, SensorContext context, File reportsDir, Project project) {
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
      coveredFiles.add(analyseHtml(moduleFileSystem, context, file));
    }
    analyseCoveredFiles(moduleFileSystem, context, coveredFiles, project);
  }

  public ErlangFileCoverage analyseHtml(ModuleFileSystem moduleFileSystem, SensorContext sensorContext,
      String testCoverageFileName) {
    File coverCoverageReportFile = new File(moduleFileSystem.baseDir(),
        getTestReportsFolder() + "/" + testCoverageFileName);
    LCOVParser parser = new LCOVParser();
    return parser.parseFile(coverCoverageReportFile);
  }

  protected void analyseCoveredFiles(ModuleFileSystem moduleFileSystem, SensorContext sensorContext,
      List<ErlangFileCoverage> coveredFiles, Project project) {

    for (File file : moduleFileSystem.files(Erlang.sourceQuery)) {
      try {
        ErlangFileCoverage fileCoverage = getFileCoverage(file, coveredFiles);
        org.sonar.api.resources.File resource = org.sonar.api.resources.File.fromIOFile(file, project);
        PropertiesBuilder<Integer, Integer> lineHitsData = new PropertiesBuilder<Integer, Integer>(
            CoreMetrics.COVERAGE_LINE_HITS_DATA);

        if (fileCoverage != null) {
          Map<Integer, Integer> hits = fileCoverage.getLineCoverageData();
          for (Map.Entry<Integer, Integer> entry : hits.entrySet()) {
            lineHitsData.add(entry.getKey(), entry.getValue());
          }

          sensorContext.saveMeasure(resource, lineHitsData.build());
          sensorContext.saveMeasure(resource, CoreMetrics.LINES_TO_COVER,
              (double) fileCoverage.getLinesToCover());
          sensorContext.saveMeasure(resource, CoreMetrics.UNCOVERED_LINES,
              (double) fileCoverage.getUncoveredLines());
        } else {

          // colour all lines as not executed
          for (int x = 1; x < sensorContext.getMeasure(resource, CoreMetrics.LINES)
              .getIntValue(); x++) {
            lineHitsData.add(x, 0);
          }

          // use non comment lines of code for coverage calculation
          Measure ncloc = sensorContext.getMeasure(resource, CoreMetrics.NCLOC);
          sensorContext.saveMeasure(resource, lineHitsData.build());
          sensorContext.saveMeasure(resource, CoreMetrics.LINES_TO_COVER, ncloc
              .getValue());
          sensorContext.saveMeasure(resource, CoreMetrics.UNCOVERED_LINES, ncloc
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
    return erlang.getConfiguration().getString(ErlangPlugin.EUNIT_FOLDER_KEY,
        ErlangPlugin.EUNIT_DEFAULT_FOLDER);
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
