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
package org.sonar.plugins.erlang.xref;

import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.plugins.erlang.languages.ErlangLanguage;
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
            .onlyOnLanguage(ErlangLanguage.KEY)
            .name("Erlang Xref Sensor");
  }

  @Override
  public void execute(SensorContext context) {
    new XrefReportParser(context).xref(xrefRuleManager);
  }
}
