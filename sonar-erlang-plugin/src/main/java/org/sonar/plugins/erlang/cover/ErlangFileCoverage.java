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

import java.util.HashMap;
import java.util.Map;

public final class ErlangFileCoverage {

  private Map<Integer, Integer> lineCoverageData = new HashMap<Integer, Integer>();
  private String filePath;

  public Map<Integer, Integer> getLineCoverageData() {
    return lineCoverageData;
  }

  public void setLineCoverage(Map<Integer, Integer> lineCoverage) {
    this.lineCoverageData = lineCoverage;
  }

  public String getFilePath() {
    return filePath;
  }

  public void setFilePath(String filePath) {
    this.filePath = filePath;
  }

  // Executable Line Count
  public int getLinesToCover() {
    return lineCoverageData.size();
  }

  // Covered Executable Line Count
  public int getCoveredLines() {
    int lines = 0;
    for (Map.Entry<Integer, Integer> entry : lineCoverageData.entrySet()) {
      if (entry.getValue() > 0) {
        lines++;
      }
    }
    return lines;
  }

  public void addLine(int lineNumber, int executionCount) {
    lineCoverageData.put(lineNumber, executionCount);
  }

  public int getUncoveredLines() {
    return getLinesToCover() - getCoveredLines();
  }

}
