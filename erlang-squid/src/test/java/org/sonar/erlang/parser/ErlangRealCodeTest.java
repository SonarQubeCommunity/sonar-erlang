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
package org.sonar.erlang.parser;

import org.sonar.erlang.ErlangConfiguration;
import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.parser.ErlangParser;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ExtendedStackTrace;
import com.sonar.sslr.impl.events.ExtendedStackTraceStream;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangRealCodeTest {
  ExtendedStackTrace listener = new ExtendedStackTrace();
  Parser<ErlangGrammar> p = ErlangParser
      .create(new ErlangConfiguration(Charsets.UTF_8), listener);

  ErlangGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.module);
  }

  @Test
  public void realLife2() throws IOException, URISyntaxException {
    assertThat(p).matches((readFromFile("user_auth_mnesia.erl")));
  }

  @Test
  public void realLife3() throws IOException, URISyntaxException {
    assertThat(p).matches((readFromFile("agner_main_sub.erl")));
  }

  @Test
  public void realLife4() throws IOException, URISyntaxException {
    assertThat(p).matches((readFromFile("erl_img.erl")));
  }

  @Test
  public void realLife5() throws IOException, URISyntaxException {
    assertThat(p).matches((readFromFile("egs_proto.erl")));
  }

  @Test
  public void realLife6() throws IOException, URISyntaxException {
    assertThat(p).matches((readFromFile("megaco.erl")));
  }

  private String readFromFile(String fileName) throws IOException, URISyntaxException {
    StringBuilder text = new StringBuilder();
    File file = new File(ErlangRealCodeTest.class.getClassLoader().getResource(fileName)
        .toURI());
    BufferedReader reader = Files.newReader(file, Charsets.UTF_8);
    String line = null;
    while ((line = reader.readLine()) != null) {
      text.append(line).append("\n");
    }
    return text.toString();
  }

  @After
  public void log() {
    ExtendedStackTraceStream.print(listener, System.out);
  }

}
