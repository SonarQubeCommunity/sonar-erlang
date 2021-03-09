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
package org.sonar.erlang.sslr.toolkit;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.sonar.colorizer.*;
import org.sonar.erlang.ErlangConfiguration;
import org.sonar.sslr.parser.ParserAdapter;

import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class ErlangConfigurationModelTest {

  ErlangConfigurationModel configurationModel;

  @Before
  public void setUp() {
    configurationModel = new ErlangConfigurationModel();
  }


  @Test
  public void getCharset() {
    // should return UTF-8 by default
    Assert.assertEquals(
            StandardCharsets.UTF_8,
            configurationModel.getCharset()
    );
  }

  @Test
  public void doGetParser() {
    Assert.assertEquals(
            ParserAdapter.class,
            configurationModel.doGetParser().getClass()
    );
  }

  @Test
  public void doGetTokenizers() {
    Set<Object> expectedTokenizers = Set.of(
            StringTokenizer.class,
            CDocTokenizer.class,
            JavadocTokenizer.class,
            CppDocTokenizer.class,
            KeywordsTokenizer.class);

    HashSet<Object> tokenizers = new HashSet<>(configurationModel
            .getTokenizers()
            .stream()
            .map(Tokenizer::getClass).collect(Collectors.toSet()));

    Assert.assertEquals(expectedTokenizers, tokenizers);
  }

  @Test
  public void getProperties() {
    Assert.assertEquals(
            "Charset",
            configurationModel.charsetProperty.getName()
    );
    Assert.assertEquals(
            "sonar.sourceEncoding",
            configurationModel.charsetProperty.getDescription()
    );
    // Assert that encoding is set to UTF-8 by default
    Assert.assertEquals(
            "UTF-8",
            configurationModel.charsetProperty.getValue()
    );
  }

  @Test
  public void getConfiguration() {
    Assert.assertEquals(
            new ErlangConfiguration(StandardCharsets.UTF_8).getCharset(),
            configurationModel.getConfiguration().getCharset()
    );
  }

  @Test
  public void getPropertyOrDefaultValue() {
    // Test when sourceEncoding property not set
    Assert.assertEquals(
            "UTF-8",
            ErlangConfigurationModel.getPropertyOrDefaultValue()
    );

    // Test when sourceEncoding property is set to some other encoding
    System.setProperty("sonar.sourceEncoding", "UTF-16");
    Assert.assertEquals(
            "UTF-16",
            ErlangConfigurationModel.getPropertyOrDefaultValue()
    );
  }

  @After
  public void cleanUp() {
    System.clearProperty("sonar.sourceEncoding");
  }
}
