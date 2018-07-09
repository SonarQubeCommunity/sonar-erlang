/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2018 Tamas Kende; Denes Hegedus (Cursor Insight Ltd.)
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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.plugins.erlang.eunit;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlText;
import com.google.common.collect.Lists;

import java.util.List;

public final class EunitTestCase {
  @JacksonXmlProperty(isAttribute = true)
  private float time;

  @JacksonXmlProperty(isAttribute = true)
  private String name;

  @JacksonXmlProperty
  private String failure;

  @JacksonXmlProperty(localName = "system-out")
  private String systemOut;

  class Failure {
    @JacksonXmlProperty(isAttribute = true)
    private String type;
    @JacksonXmlText(value = true)
    private String error;
  }

}
