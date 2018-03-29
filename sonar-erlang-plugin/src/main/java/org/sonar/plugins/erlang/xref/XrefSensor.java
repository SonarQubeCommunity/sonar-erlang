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
package org.sonar.plugins.erlang.xref;

import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.plugins.erlang.core.Erlang;
import org.sonar.plugins.erlang.dialyzer.ErlangRuleManager;

/**
 * Calls the xref report parser saves violations to sonar
 *
 * @author tkende
 */
public class XrefSensor implements Sensor {

  private ErlangRuleManager xrefRuleManager;

  public XrefSensor() {
    xrefRuleManager = new ErlangRuleManager(XrefRuleDefinition.XREF_PATH);
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor
            .onlyOnLanguage(Erlang.KEY)
            .name("Erlang Xref Sensor");
  }

  @Override
  public void execute(SensorContext context) {
    new XrefReportParser(context).xref(xrefRuleManager);
  }
}
