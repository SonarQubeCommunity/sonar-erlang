/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2016 Tamas Kende
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

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.config.Settings;
import org.sonar.api.resources.DuplicatedSourceException;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Qualifiers;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.io.IOException;

public class EunitXmlSensor implements Sensor {

  private static final Logger LOG = LoggerFactory.getLogger(EunitXmlSensor.class);

  protected Erlang erlang;
  protected final Settings settings;
  private FileSystem fileSystem;
  private final FilePredicate mainFilePredicate;
  private final FilePredicate testFilePredicate;

  public EunitXmlSensor(Erlang erlang, FileSystem fileSystem, Settings settings) {
    this.erlang = erlang;
    this.settings = settings;
    this.fileSystem = fileSystem;
    this.mainFilePredicate = fileSystem.predicates().and(
            fileSystem.predicates().hasType(InputFile.Type.MAIN),
            fileSystem.predicates().hasLanguage(Erlang.KEY));
    this.testFilePredicate = fileSystem.predicates().and(
            fileSystem.predicates().hasType(InputFile.Type.TEST),
            fileSystem.predicates().hasLanguage(Erlang.KEY));
  }

  @Override
  public boolean shouldExecuteOnProject(Project project) {
    return fileSystem.hasFiles(mainFilePredicate);
  }

  @Override
  public void analyse(Project project, SensorContext context) {
    String eunitFolder = settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY);
    try {
      collect(project, context,
              new File(fileSystem.baseDir(), eunitFolder));
    } catch (Exception e) {
      LOG.error("Error occured during eunit xml file parsing", e.getMessage(), e);
    }
  }

  protected void collect(final Project project, final SensorContext context, File reportsDir) {
    LOG.debug("Parsing Eunit run results in Surefile format from folder {}", reportsDir);
    if (reportsDir.exists() && fileSystem.hasFiles(testFilePredicate)) {
      new AbstractSurefireParser() {

        @Override
        protected Resource getUnitTestResource(String classKey) {
          File unitTestFile = getUnitTestFile(fileSystem.files(testFilePredicate), fileSystem.files(mainFilePredicate), classKey);

          org.sonar.api.resources.File unitTestFileResource = getUnitTestFileResource(unitTestFile);
          unitTestFileResource.setLanguage(erlang);
          unitTestFileResource.setQualifier(Qualifiers.UNIT_TEST_FILE);

          LOG.debug("Adding unittest resource: {}", unitTestFileResource.toString());

          String source;

          try {
            source = FileUtils.readFileToString(unitTestFile, fileSystem.encoding());
          } catch (IOException e) {
            source = "Could not find source for unit test: " + classKey
                    + " in any of test directories";
            LOG.debug(source, e);
          }

          try {
            context.saveSource(unitTestFileResource, source);
          } catch (DuplicatedSourceException e) {
            unitTestFileResource = org.sonar.api.resources.File.create(unitTestFile.getPath());
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

  protected org.sonar.api.resources.File getUnitTestFileResource(File unitTestFile) {
    InputFile inputFile = fileSystem.inputFile(fileSystem.predicates().is(unitTestFile));
    return org.sonar.api.resources.File.create(inputFile.relativePath());
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

  protected File getUnitTestFile(Iterable<File> testFiles, Iterable<File> srcFiles, String name) {
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

  private File findFileByName(Iterable<File> testFiles, String fileName) {
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
