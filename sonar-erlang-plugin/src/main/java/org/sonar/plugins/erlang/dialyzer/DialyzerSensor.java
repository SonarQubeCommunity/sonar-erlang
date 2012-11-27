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
package org.sonar.plugins.erlang.dialyzer;

import org.sonar.api.batch.SensorContext;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.plugins.erlang.core.Erlang;

/**
 * Calls the dialyzer report parser saves violations to sonar
 *
 * @author tkende
 *
 */
public class DialyzerSensor extends AbstractErlangSensor {

  private ErlangRuleManager dialyzerRuleManager = new ErlangRuleManager(
      DialyzerRuleRepository.DIALYZER_PATH);
  private RulesProfile rulesProfile;

  public DialyzerSensor(Erlang erlang, RulesProfile rulesProfile) {
    super(erlang);
    this.rulesProfile = rulesProfile;
  }

  public void analyse(Project project, SensorContext context) {
    new DialyzerReportParser().dialyzer(project, context, dialyzerRuleManager, rulesProfile);
  }
}
