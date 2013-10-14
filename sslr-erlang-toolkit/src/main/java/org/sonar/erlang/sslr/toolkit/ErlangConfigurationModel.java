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
package org.sonar.erlang.sslr.toolkit;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.Grammar;
import com.sonar.sslr.impl.Parser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.colorizer.CDocTokenizer;
import org.sonar.colorizer.CppDocTokenizer;
import org.sonar.colorizer.JavadocTokenizer;
import org.sonar.colorizer.KeywordsTokenizer;
import org.sonar.colorizer.StringTokenizer;
import org.sonar.colorizer.Tokenizer;
import org.sonar.erlang.ErlangConfiguration;
import org.sonar.erlang.api.ErlangKeyword;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.sslr.parser.LexerlessGrammar;
import org.sonar.sslr.parser.ParserAdapter;
import org.sonar.sslr.toolkit.AbstractConfigurationModel;
import org.sonar.sslr.toolkit.ConfigurationProperty;
import org.sonar.sslr.toolkit.Validators;

import java.nio.charset.Charset;
import java.util.List;

public class ErlangConfigurationModel extends AbstractConfigurationModel {

  private static final Logger LOG = LoggerFactory
    .getLogger(ErlangConfigurationModel.class);
  private static final String CHARSET_PROPERTY_KEY = "sonar.sourceEncoding";

  @VisibleForTesting
  ConfigurationProperty charsetProperty = new ConfigurationProperty("Charset",
    CHARSET_PROPERTY_KEY, getPropertyOrDefaultValue(CHARSET_PROPERTY_KEY, "UTF-8"),
    Validators.charsetValidator());

  @Override
  public Charset getCharset() {
    return Charset.forName(charsetProperty.getValue());
  }

  @Override
  public Parser<? extends Grammar> doGetParser() {
    return new ParserAdapter<LexerlessGrammar>(getCharset(), ErlangGrammarImpl.createGrammar());
  }

  @Override
  public List<Tokenizer> doGetTokenizers() {
    return ImmutableList.of(
      new StringTokenizer("<span class=\"s\">", "</span>"),
      new CDocTokenizer("<span class=\"cd\">", "</span>"),
      new JavadocTokenizer("<span class=\"cppd\">", "</span>"),
      new CppDocTokenizer("<span class=\"cppd\">", "</span>"),
      new KeywordsTokenizer("<span class=\"k\">", "</span>", ErlangKeyword.keywordValues()));
  }

  public List<ConfigurationProperty> getProperties() {
    return ImmutableList.of(charsetProperty);
  }

  @VisibleForTesting
  ErlangConfiguration getConfiguration() {
    return new ErlangConfiguration(getCharset());
  }

  @VisibleForTesting
  static String getPropertyOrDefaultValue(String propertyKey,
    String defaultValue) {
    String propertyValue = System.getProperty(propertyKey);

    if (propertyValue == null) {
      LOG.info("The property \"" + propertyKey + "\" is not set, using the default value \"" + defaultValue + "\".");
      return defaultValue;
    } else {
      LOG.info("The property \"" + propertyKey + "\" is set, using its value \"" + propertyValue + "\".");
      return propertyValue;
    }
  }

}
