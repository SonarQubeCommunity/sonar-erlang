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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;


public class CoverDataFileParserTest {

  private File eunitCoverDataTestFile, commonTestCoverDataTestFile;

  @Before
  public void setUp() {
    eunitCoverDataTestFile = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/.eunit/eunit.coverdata");
    commonTestCoverDataTestFile = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/all.coverdata");
  }

  @Test
  public void eunitParseTest() throws IOException {
    List<ErlangFileCoverage> coverageResult = CoverDataFileParser.parse(eunitCoverDataTestFile);
    Assert.assertNotNull(coverageResult.get(0));

    ErlangFileCoverage moduleCoverage = getCoverageResultForModule("erlcount_lib.erl", coverageResult);

    Assert.assertNotNull(moduleCoverage);
    Assert.assertEquals(19, moduleCoverage.getCoveredLines());
    Assert.assertEquals(21, moduleCoverage.getLinesToCover());
    Assert.assertEquals(2, moduleCoverage.getUncoveredLines());
  }

  @Test
  public void commonTestParseTest() throws IOException {
    List<ErlangFileCoverage> coverageResult = CoverDataFileParser.parse(commonTestCoverDataTestFile);
    Assert.assertNotNull(coverageResult.get(0));

    ErlangFileCoverage moduleCoverage = getCoverageResultForModule("erlcount_lib.erl", coverageResult);

    Assert.assertNotNull(moduleCoverage);
    Assert.assertEquals(17, moduleCoverage.getCoveredLines());
    Assert.assertEquals(21, moduleCoverage.getLinesToCover());
    Assert.assertEquals(4, moduleCoverage.getUncoveredLines());
  }

  private ErlangFileCoverage getCoverageResultForModule(String module, List<ErlangFileCoverage> cov) {
    return cov
            .stream()
            .filter(erlangFileCoverage ->
                    erlangFileCoverage.getFilePath().equals(module))
            .collect(Collectors.toList())
            .get(0);
  }
}

