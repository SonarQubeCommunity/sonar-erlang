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
package org.sonar.plugins.erlang.eunit;

import com.google.common.collect.Maps;

import java.util.Map;

public class UnitTestIndex {

  private Map<String, UnitTestClassReport> indexByClassname;

  public UnitTestIndex() {
    this.indexByClassname = Maps.newHashMap();
  }

  public UnitTestClassReport index(String classname) {
    UnitTestClassReport classReport = indexByClassname.get(classname);
    if (classReport == null) {
      classReport = new UnitTestClassReport();
      indexByClassname.put(classname, classReport);
    }
    return classReport;
  }
  public Map<String, UnitTestClassReport> getIndexByClassname() {
    return indexByClassname;
  }

  public int size() {
    return indexByClassname.size();
  }

}
