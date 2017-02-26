/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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
package org.sonar.plugins.erlang.dialyzer;

import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.plugins.erlang.core.Erlang;

/**
 * Calls the dialyzer report parser saves violations to sonar
 *
 * @author tkende
 */
public class DialyzerSensor implements Sensor {

  private final SensorContext context;
  private ErlangRuleManager dialyzerRuleManager = new ErlangRuleManager(DialyzerRuleDefinition.DIALYZER_PATH);

  public DialyzerSensor(SensorContext context) {
    this.context = context;
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor
            .onlyOnLanguage(Erlang.KEY)
            .name("Erlang Dialyzer Sensor")
            .onlyOnFileType(InputFile.Type.MAIN);
  }

  @Override
  public void execute(org.sonar.api.batch.sensor.SensorContext context) {
    new DialyzerReportParser(context).dialyzer();
  }
}
