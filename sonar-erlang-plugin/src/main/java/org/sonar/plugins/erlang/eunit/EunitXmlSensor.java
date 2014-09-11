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
package org.sonar.plugins.erlang.eunit;

import org.apache.commons.io.FileUtils;
import org.jfree.util.Log;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.config.Settings;
import org.sonar.api.resources.DuplicatedSourceException;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Qualifiers;
import org.sonar.api.resources.Resource;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;
import org.sonar.plugins.surefire.api.AbstractSurefireParser;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class EunitXmlSensor implements Sensor {

  protected Erlang erlang;
  protected final Settings settings;
  private ModuleFileSystem moduleFileSystem;

  public EunitXmlSensor(Erlang erlang, ModuleFileSystem moduleFileSystem, Settings settings) {
    this.erlang = erlang;
    this.settings = settings;
    this.moduleFileSystem = moduleFileSystem;
  }

  private static final Logger LOG = LoggerFactory.getLogger(EunitXmlSensor.class);

  public boolean shouldExecuteOnProject(Project project) {
    return !moduleFileSystem.files(Erlang.SOURCE_QUERY).isEmpty();
  }

  public void analyse(Project project, SensorContext context) {
    String eunitFolder = settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY);
    try {
      collect(project, context,
        new File(moduleFileSystem.baseDir(), eunitFolder));
    } catch (Exception e) {
      LOG.error("Error occured during eunit xml file parsing", e.getMessage(), e);
    }
  }

  protected void collect(final Project project, final SensorContext context, File reportsDir) {
    LOG.debug("Parsing Eunit run results in Surefile format from folder {}", reportsDir);
    if (reportsDir.exists() && !moduleFileSystem.testDirs().isEmpty()) {
      new AbstractSurefireParser() {

        @Override
        protected Resource getUnitTestResource(String classKey) {
          File unitTestFile = getUnitTestFile(moduleFileSystem.files(Erlang.TEST_QUERY), moduleFileSystem.files(Erlang.SOURCE_QUERY), classKey);

          org.sonar.api.resources.File unitTestFileResource = getUnitTestFileResource(unitTestFile.getName());
          unitTestFileResource.setLanguage(erlang);
          unitTestFileResource.setQualifier(Qualifiers.UNIT_TEST_FILE);

          LOG.debug("Adding unittest resource: {}", unitTestFileResource.toString());

          String source = "";

          try {
            source = FileUtils.readFileToString(unitTestFile, moduleFileSystem.sourceCharset().name());
          } catch (IOException e) {
            source = "Could not find source for unit test: " + classKey
              + " in any of test directories";
            Log.debug(source, e);
          }

          try {
            context.saveSource(unitTestFileResource, source);
          } catch (DuplicatedSourceException e) {
            unitTestFileResource = org.sonar.api.resources.File.fromIOFile(unitTestFile, project);
          }

          return unitTestFileResource;
        }

      }.collect(project, context, reportsDir);
    } else {
      LOG.debug("Eunit folder {} or test folder does not exists. Skip.", reportsDir);
    }
  }

  protected String cleanName(String name) {
    return name.replaceFirst("(.*?')(.*?)('.*)", "$2");
  }

  protected org.sonar.api.resources.File getUnitTestFileResource(String classKey) {
    return new org.sonar.api.resources.File(classKey);
  }

  protected String getUnitTestFileName(String className) {
    String fileName = cleanName(className);
    fileName = fileName.replace('.', '/');
    if (fileName.endsWith("eunit") || fileName.endsWith("tests")) {
      return fileName + ".erl";
    } else {
      return fileName + "_tests.erl";
    }
  }

  protected File getUnitTestFile(List<File> testFiles, List<File> srcFiles, String name) {
    String fileName = getUnitTestFileName(name);
    File file = findFileByName(testFiles, fileName);
    if (file == null) {
      file = findFileByName(srcFiles, fileName);
    }
    if (file == null) {
      file = new File("");
    }
    return file;
  }

  private File findFileByName(List<File> testFiles, String fileName) {
    for (File testFile : testFiles) {
      if (testFile.getAbsolutePath().endsWith(fileName) || testFile.getAbsolutePath().endsWith(fileName.replaceAll("_(eunit|tests)", ""))) {
        return testFile;
      }
    }
    return null;
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }
}
