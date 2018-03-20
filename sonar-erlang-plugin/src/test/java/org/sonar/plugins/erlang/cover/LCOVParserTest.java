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
package org.sonar.plugins.erlang.cover;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.hamcrest.Matchers;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;

public class LCOVParserTest {

  @Test
  public void checkCoverage() {
    Path coverageFile = Paths.get("src/test/resources/org/sonar/plugins/erlang/erlcount/.eunit/erlcount_lib.COVER.html");

    ErlangFileCoverage coverage = new LCOVParser().parseFile(coverageFile.toFile());

    assertThat(coverage.getCoveredLines(), Matchers.equalTo(19));
    assertThat(coverage.getLinesToCover(), Matchers.equalTo(21));
    assertThat(coverage.getUncoveredLines(), Matchers.equalTo(2));
  }

}
