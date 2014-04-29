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

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import org.hamcrest.Matchers;
import org.junit.Test;
import org.sonar.plugins.erlang.ProjectUtil;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;

public class CoverDataFileParserTest {

  @Test
  public void test() throws IOException, URISyntaxException, OtpErlangDecodeException{
    List<ErlangFileCoverage> coverageResult = CoverDataFileParser.parse(new File(ProjectUtil.class.getResource("/org/sonar/plugins/erlang/erlcount/.eunit/eunit.coverdata").toURI()));
    assertThat(coverageResult.get(0), Matchers.notNullValue());
    ErlangFileCoverage cov = getResultOfModule("erlcount_lib.erl", coverageResult);
    assertThat(cov.getCoveredLines(), Matchers.equalTo(19));
    assertThat(cov.getLinesToCover(), Matchers.equalTo(21));
    assertThat(cov.getUncoveredLines(), Matchers.equalTo(2));
  }

  private ErlangFileCoverage getResultOfModule(String module, List<ErlangFileCoverage> cov){
    for (ErlangFileCoverage erlangFileCoverage : cov) {
      if(module.equals(erlangFileCoverage.getFilePath())){
        return erlangFileCoverage;
      }
    }
    return null;
  }

}
