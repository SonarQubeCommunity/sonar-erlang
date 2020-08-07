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
package org.sonar.plugins.erlang.core;

import org.apache.commons.lang.StringUtils;
import org.sonar.api.config.Settings;
import org.sonar.api.resources.AbstractLanguage;
import org.sonar.plugins.erlang.ErlangPlugin;

public class Erlang extends AbstractLanguage {

  public static final String KEY = "erlang";

  private Settings settings;

  public Erlang(Settings settings) {
    super(KEY, "Erlang");
    this.settings = settings;
  }

  @Override
  public String[] getFileSuffixes() {
    String[] suffixes = settings.getStringArray(ErlangPlugin.FILE_SUFFIXES_KEY);
    if (suffixes == null || suffixes.length == 0) {
      suffixes = StringUtils.split(ErlangPlugin.FILE_SUFFIXES_DEFVALUE, ",");
    }
    return suffixes;
  }

}
