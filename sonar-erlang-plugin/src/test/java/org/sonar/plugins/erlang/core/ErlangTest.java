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
package org.sonar.plugins.erlang.core;

import org.junit.Before;
import org.junit.Test;
import org.sonar.api.config.Settings;
import org.sonar.plugins.erlang.ErlangPlugin;

import static org.junit.Assert.assertArrayEquals;

public class ErlangTest {

  private Settings settings;
  private Erlang erlang;

  @Before
  public void setUp() {
    settings = new Settings();
    erlang = new Erlang(settings);
  }

  @Test
  public void defaultSuffixes() {
    assertArrayEquals(erlang.getFileSuffixes(), new String[]{"erl"});
  }

  @Test
  public void customSuffixes() {
    settings.setProperty(ErlangPlugin.FILE_SUFFIXES_KEY, "erlang");

    assertArrayEquals(erlang.getFileSuffixes(), new String[]{"erlang"});
  }

}
