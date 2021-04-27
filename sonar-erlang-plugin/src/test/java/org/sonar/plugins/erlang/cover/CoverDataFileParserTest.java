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

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;

public class CoverDataFileParserTest {

  private File coverDataTestFile;

  @Before
  public void setUp() {
    coverDataTestFile = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/.eunit/eunit.coverdata");
  }

  @Test
  public void parseTest() throws IOException {
    List<ErlangFileCoverage> coverageResult = CoverDataFileParser.parse(coverDataTestFile);
    assertThat(coverageResult.get(0), Matchers.notNullValue());

    ErlangFileCoverage moduleCoverage = getCoverageResultForModule("erlcount_lib.erl", coverageResult);

    assertThat(moduleCoverage, Matchers.notNullValue());
    assertThat(moduleCoverage.getCoveredLines(), Matchers.equalTo(19));
    assertThat(moduleCoverage.getLinesToCover(), Matchers.equalTo(21));
    assertThat(moduleCoverage.getUncoveredLines(), Matchers.equalTo(2));
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

