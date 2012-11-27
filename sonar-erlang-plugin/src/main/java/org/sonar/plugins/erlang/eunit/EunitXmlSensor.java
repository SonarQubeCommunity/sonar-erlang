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
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Qualifiers;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;
import org.sonar.plugins.surefire.api.AbstractSurefireParser;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class EunitXmlSensor implements Sensor {

  protected Erlang erlang;

  public EunitXmlSensor(Erlang erlang) {
    this.erlang = erlang;
  }

  private static final Logger LOG = LoggerFactory.getLogger(EunitXmlSensor.class);

  public boolean shouldExecuteOnProject(Project project) {
    return (erlang.equals(project.getLanguage()));
  }

  public void analyse(Project project, SensorContext context) {
    String jsTestDriverFolder = erlang.getConfiguration().getString(
        ErlangPlugin.EUNIT_FOLDER_KEY, ErlangPlugin.EUNIT_DEFAULT_FOLDER);
    collect(project, context,
        new File(project.getFileSystem().getBasedir(), jsTestDriverFolder));
  }

  protected void collect(final Project project, final SensorContext context, File reportsDir) {
    LOG.debug("Parsing Eunit run results in Surefile format from folder {}", reportsDir);

    new AbstractSurefireParser() {

      @Override
      protected Resource<?> getUnitTestResource(String classKey) {

        org.sonar.api.resources.File unitTestFileResource = getUnitTestFileResource(classKey);
        unitTestFileResource.setLanguage(erlang);
        unitTestFileResource.setQualifier(Qualifiers.UNIT_TEST_FILE);

        LOG.debug("Adding unittest resource: {}", unitTestFileResource.toString());

        List<File> testDirectories = project.getFileSystem().getTestDirs();

        File unitTestFile = getUnitTestFile(testDirectories, getUnitTestFileName(classKey));

        String source = "";

        try {
          source = FileUtils.readFileToString(unitTestFile, project.getFileSystem()
              .getSourceCharset().name());
        } catch (IOException e) {
          source = "Could not find source for unit test: " + classKey
            + " in any of test directories";
          Log.debug(source, e);
        }

        context.saveSource(unitTestFileResource, source);

        return unitTestFileResource;
      }
    }.collect(project, context, reportsDir);

  }

  protected String cleanName(String name) {
    return name.replaceFirst("(.*?')(.*?)('.*)", "$2");
  }

  protected org.sonar.api.resources.File getUnitTestFileResource(String classKey) {
    return new org.sonar.api.resources.File(cleanName(classKey).replaceAll("\\.", "/") + ".erl");
  }

  protected String getUnitTestFileName(String className) {
    String fileName = cleanName(className);
    fileName = fileName.replace('.', '/');
    fileName = fileName + ".erl";
    return fileName;
  }

  protected File getUnitTestFile(List<File> testDirectories, String name) {
    File unitTestFile = new File("");
    for (File dir : testDirectories) {
      unitTestFile = new File(dir, name);

      if (unitTestFile.exists()) {
        break;
      }
    }
    return unitTestFile;
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }
}
