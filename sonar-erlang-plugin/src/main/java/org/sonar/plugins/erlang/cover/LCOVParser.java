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

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public final class LCOVParser {

  private static final String COVERAGE_DATA_REGEX = "(.*?)([0-9]+)(\\.\\..*?)";
  private static final Logger LOG = LoggerFactory.getLogger(LCOVParser.class);

  public ErlangFileCoverage parseFile(File file) {
    List<String> lines = new LinkedList<String>();
    try {
      lines = FileUtils.readLines(file);
    } catch (IOException e) {
      LOG.debug("Cound not read content from file: " + file.getName());
    }

    ErlangFileCoverage fileCoverage = new ErlangFileCoverage();
    boolean started = false;
    int lineNumber = 1;
    for (String line : lines) {
      if (!started && line.matches("File generated from .*")) {
        String fileName = line.replaceFirst("(File generated from )(.*?)( by .*)", "$2");
        fileName = fileName.substring(fileName.lastIndexOf("/") + 1);
        fileCoverage = new ErlangFileCoverage();
        fileCoverage.setFilePath(fileName);
      }
      if (line.indexOf("**************") > -1) {
        started = true;
      }
      if (started && line.matches(".*?\\|.*")) {
        String[] lineData = line.split("\\|", 2);
        if (!StringUtils.isBlank(lineData[0].trim())) {
          String executionCount = lineData[0].trim()
              .replaceAll(COVERAGE_DATA_REGEX, "$2");
          fileCoverage.addLine(lineNumber, Integer.valueOf(executionCount).intValue());
        }
        lineNumber++;
      }
    }
    return fileCoverage;
  }
}
