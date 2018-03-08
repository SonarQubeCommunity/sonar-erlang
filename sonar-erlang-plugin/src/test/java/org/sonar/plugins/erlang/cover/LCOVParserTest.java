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

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.sonar.test.TestUtils;

import static org.hamcrest.MatcherAssert.assertThat;

public class LCOVParserTest {

  private ErlangFileCoverage cov;
  
  @Before
  public void setup() throws URISyntaxException, IOException {
    File testResourcesBasDir = new File("src/test/resources/");
    String coverageFile = "org/sonar/plugins/erlang/erlcount/.eunit/erlcount_lib.COVER.html";

    cov = new LCOVParser().parseFile(new File(testResourcesBasDir.toPath().resolve(coverageFile).toString()));
  }

  @Test
  public void checkCoverage() {
    assertThat(cov.getCoveredLines(), Matchers.equalTo(19));
    assertThat(cov.getLinesToCover(), Matchers.equalTo(21));
    assertThat(cov.getUncoveredLines(), Matchers.equalTo(2));
  }

}
