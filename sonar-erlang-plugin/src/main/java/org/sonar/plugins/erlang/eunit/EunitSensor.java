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
package org.sonar.plugins.erlang.eunit;

import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;

public class EunitSensor extends EunitXmlSensor {

  public EunitSensor(Erlang erlang) {
    super(erlang);
  }

  @Override
  public boolean shouldExecuteOnProject(Project project) {
    return (erlang.equals(project.getLanguage()));
  }

  @Override
  public void analyse(Project project, SensorContext context) {
    String eunitFolder = erlang.getConfiguration().getString(ErlangPlugin.EUNIT_FOLDER_KEY,
        ErlangPlugin.EUNIT_DEFAULT_FOLDER);
    collect(project, context, new File(project.getFileSystem().getBasedir(), eunitFolder));
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }
}
