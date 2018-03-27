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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import org.sonar.api.batch.fs.InputFile;

import java.io.File;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
@JacksonXmlRootElement(localName = "testsuite")
public class EunitTestsuite {
  @JacksonXmlProperty(isAttribute = true)
  private int errors;

  @JacksonXmlProperty(isAttribute = true)
  private int failures;

  @JacksonXmlProperty(isAttribute = true)
  private int skipped;

  @JacksonXmlProperty(isAttribute = true)
  private int tests;

  @JacksonXmlProperty(isAttribute = true)
  private float time;

  @JacksonXmlProperty(isAttribute = true)
  private String name = "";

  public String getModule() {
    return name.replaceAll(".*'(.*?)'.*", "$1");
  }

  public String getApp() {
    return name.replaceAll("file \"(.*?)\\.app\"", "$1");
  }

  public int getErrors() {
    return errors;
  }

  public int getFailures() {
    return failures;
  }

  public int getSkipped() {
    return skipped;
  }

  public int getTests() {
    return tests;
  }

  public long getTimeInMs() {
    return (long) (time * 1000);
  }

  public static EunitTestsuite find(InputFile file, List<EunitTestsuite> testReports) {
    for (EunitTestsuite testReport : testReports) {
      if (file.absolutePath().endsWith(File.separator + testReport.getModule() + ".erl")
              || file.absolutePath().endsWith(File.separator + testReport.getApp() + ".erl")) {
        return testReport;
      }
    }
    return null;
  }
}
