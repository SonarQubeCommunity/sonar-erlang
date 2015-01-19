/*
 * SonarQube Erlang Plugin
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
package org.sonar.plugins.erlang.colorizer;

import com.google.common.collect.ImmutableList;
import org.sonar.api.web.CodeColorizerFormat;
import org.sonar.colorizer.CDocTokenizer;
import org.sonar.colorizer.CppDocTokenizer;
import org.sonar.colorizer.JavadocTokenizer;
import org.sonar.colorizer.KeywordsTokenizer;
import org.sonar.colorizer.StringTokenizer;
import org.sonar.colorizer.Tokenizer;
import org.sonar.erlang.api.ErlangKeyword;
import org.sonar.plugins.erlang.core.Erlang;

import java.util.List;

public class ErlangColorizerFormat extends CodeColorizerFormat {
  private static final String END_TAG = "</span>";
  public ErlangColorizerFormat() {
    super(Erlang.KEY);
  }

  @Override
  public List<Tokenizer> getTokenizers() {
    return ImmutableList.of(new StringTokenizer("<span class=\"s\">", END_TAG),
      new CDocTokenizer("<span class=\"cd\">", END_TAG), new JavadocTokenizer(
      "<span class=\"cppd\">", END_TAG), new CppDocTokenizer(
      "<span class=\"cppd\">", END_TAG), new KeywordsTokenizer(
      "<span class=\"k\">", END_TAG, ErlangKeyword.keywordValues()));
  }

}
