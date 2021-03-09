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
package org.sonar.plugins.erlang.settings;

import java.util.Collections;
import java.util.List;
import org.sonar.api.config.PropertyDefinition;
import org.sonar.api.resources.Qualifiers;

import static java.util.Arrays.asList;

public class ErlangLanguageProperties {

  public static final String FILE_SUFFIXES_KEY = "sonar.erlang.file.suffixes";
  public static final String FILE_SUFFIXES_DEFAULT_VALUE = ".erl";

  private ErlangLanguageProperties() {
    // only statics
  }

  public static List<PropertyDefinition> getProperties() {
    return Collections.singletonList(PropertyDefinition.builder(FILE_SUFFIXES_KEY)
            .multiValues(true)
            .defaultValue(FILE_SUFFIXES_DEFAULT_VALUE)
            .category("Erlang")
            .name("File Suffixes")
            .description("Comma-separated list of suffixes for files to analyze.")
            .onQualifiers(Qualifiers.PROJECT)
            .build());
  }

}
