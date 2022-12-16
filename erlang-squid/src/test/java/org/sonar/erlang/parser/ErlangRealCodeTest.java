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
package org.sonar.erlang.parser;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangRealCodeTest {
  private final LexerlessGrammar b = ErlangGrammarImpl.createGrammar();

  @Test
  public void realLife2() throws IOException, URISyntaxException {
    assertThat(b.getRootRule()).matches((readFromFile("user_auth_mnesia.erl")));
  }

  @Test
  public void realLife3() throws IOException, URISyntaxException {
    assertThat(b.getRootRule()).matches((readFromFile("agner_main_sub.erl")));
  }

  @Test
  public void realLife4() throws IOException, URISyntaxException {
    assertThat(b.getRootRule()).matches((readFromFile("erl_img.erl")));
  }

  @Test
  public void realLife5() throws IOException, URISyntaxException {
    assertThat(b.getRootRule()).matches((readFromFile("egs_proto.erl")));
  }

  @Test
  public void realLife6() throws IOException, URISyntaxException {
    assertThat(b.getRootRule()).matches((readFromFile("megaco.erl")));
  }

  @Test
  public void parseAdvancedMacroAccess() throws IOException, URISyntaxException {
    assertThat(b.getRootRule()).matches((readFromFile("macro_test.erl")));
  }

  private String readFromFile(String fileName) throws IOException, URISyntaxException {
    StringBuilder text = new StringBuilder();
    File file = new File(ErlangRealCodeTest.class.getClassLoader().getResource(fileName)
      .toURI());
    BufferedReader reader = Files.newReader(file, Charsets.UTF_8);
    String line;
    while ((line = reader.readLine()) != null) {
      text.append(line).append("\n");
    }
    return text.toString();
  }

}
