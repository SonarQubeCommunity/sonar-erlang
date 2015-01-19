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
import org.sonar.api.resources.Language;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

public class ErlangSourceImporterTest {

  private Erlang erlang;
  private ErlangSourceImporter sourceImporter;

  @Before
  public void setUp() throws Exception {
    erlang = new Erlang(new Settings());
    sourceImporter = new ErlangSourceImporter(erlang);
  }

  @Test
  public void testCreateImporter() throws Exception {
    assertThat(sourceImporter.getLanguage(), is((Language) erlang));
  }

  @Test
  public void testToString() throws Exception {
    assertNotNull(sourceImporter.toString());
  }
}
