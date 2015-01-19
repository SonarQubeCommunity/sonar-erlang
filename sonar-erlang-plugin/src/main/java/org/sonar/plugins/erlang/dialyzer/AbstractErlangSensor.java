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
package org.sonar.plugins.erlang.dialyzer;

import org.sonar.api.batch.Sensor;
import org.sonar.api.config.Settings;
import org.sonar.api.resources.Project;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.core.Erlang;

/**
 * This is a helper base class for sensors that should only be executed on
 * Erlang projects.
 *
 * @since 0.1
 */
public abstract class AbstractErlangSensor implements Sensor {

  private final Settings settings;
  protected ModuleFileSystem moduleFileSystem;

  protected AbstractErlangSensor(ModuleFileSystem fileSystem, Settings settings) {
    this.settings = settings;
    this.moduleFileSystem = fileSystem;
  }

  @Override
  public final boolean shouldExecuteOnProject(Project project) {
    return !moduleFileSystem.files(Erlang.SOURCE_QUERY).isEmpty();
  }

  public final Settings getSettings() {
    return settings;
  }
}
