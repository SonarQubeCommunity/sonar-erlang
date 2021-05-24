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

import java.util.HashMap;
import java.util.Map;

final class ErlangFileCoverage {

  private final Map<Integer, Integer> lineCoverageData = new HashMap<>();
  private String filePath;

  Map<Integer, Integer> getLineCoverageData() {
    return lineCoverageData;
  }

  String getFilePath() {
    return filePath;
  }

  void setFilePath(String filePath) {
    this.filePath = filePath;
  }

  // Executable Line Count
  int getLinesToCover() {
    return lineCoverageData.size();
  }

  // Covered Executable Line Count
  int getCoveredLines() {
    return (int) lineCoverageData.entrySet().stream().filter(entry -> entry.getValue() > 0).count();
  }

  void addLine(int lineNumber, int executionCount) {
    lineCoverageData.put(lineNumber, executionCount);
  }

  int getUncoveredLines() {
    return getLinesToCover() - getCoveredLines();
  }

}
