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
package org.sonar.plugins.erlang.core;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.sonar.plugins.erlang.ErlangPlugin;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ErlangTest {

  private Configuration configuration;
  private Erlang erlang;

  @Before
  public void setUp() {
    configuration = mock(Configuration.class);
    erlang = new Erlang(configuration);
  }

  @Test
  public void defaultSuffixes() {
    when(configuration.getStringArray(ErlangPlugin.FILE_SUFFIXES_KEY)).thenReturn(null)
        .thenReturn(new String[] {});
    assertArrayEquals(erlang.getFileSuffixes(), new String[] {"erl"});
    assertArrayEquals(erlang.getFileSuffixes(), new String[] {"erl"});
    assertSame(configuration, erlang.getConfiguration());
  }

  @Test
  public void customSuffixes() {
    when(configuration.getStringArray(ErlangPlugin.FILE_SUFFIXES_KEY)).thenReturn(
        new String[] {"erlang"});
    assertArrayEquals(erlang.getFileSuffixes(), new String[] {"erlang"});
  }

}
