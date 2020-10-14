/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
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

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
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

  private static final Logger LOG = LoggerFactory.getLogger(ErlangConfigurationModel.class);
  private static final String CHARSET_PROPERTY_KEY = "sonar.sourceEncoding";
  private static final String END_TAG = "</span>";

  @VisibleForTesting
  ConfigurationProperty charsetProperty = new ConfigurationProperty("Charset",
    CHARSET_PROPERTY_KEY, getPropertyOrDefaultValue(CHARSET_PROPERTY_KEY, "UTF-8"),
    Validators.charsetValidator());

  @Override
  public Charset getCharset() {
    return Charset.forName(charsetProperty.getValue());
  }

  @Override
  public Parser doGetParser() {
    return new ParserAdapter<LexerlessGrammar>(getCharset(), ErlangGrammarImpl.createGrammar());
  }

  @Override
  public List<Tokenizer> doGetTokenizers() {
    return ImmutableList.of(
      new StringTokenizer("<span class=\"s\">", END_TAG),
      new CDocTokenizer("<span class=\"cd\">", END_TAG),
      new JavadocTokenizer("<span class=\"cppd\">", END_TAG),
      new CppDocTokenizer("<span class=\"cppd\">", END_TAG),
      new KeywordsTokenizer("<span class=\"k\">", END_TAG, ErlangKeyword.keywordValues()));
  }

  @Override
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
