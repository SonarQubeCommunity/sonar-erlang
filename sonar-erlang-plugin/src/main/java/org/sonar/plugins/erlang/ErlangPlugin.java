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
package org.sonar.plugins.erlang;

import com.google.common.collect.ImmutableList;
import org.sonar.api.Extension;
import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.api.SonarPlugin;
import org.sonar.plugins.erlang.checks.ErlangChecksRuleRepository;
import org.sonar.plugins.erlang.colorizer.ErlangColorizerFormat;
import org.sonar.plugins.erlang.core.Erlang;
import org.sonar.plugins.erlang.core.ErlangSourceImporter;
import org.sonar.plugins.erlang.cover.CoverCoverageSensor;
import org.sonar.plugins.erlang.cpd.ErlangCpdMapping;
import org.sonar.plugins.erlang.dialyzer.DialyzerRuleRepository;
import org.sonar.plugins.erlang.dialyzer.DialyzerSensor;
import org.sonar.plugins.erlang.eunit.EunitSensor;

import java.util.List;

@Properties({
  @Property(
    key = ErlangPlugin.FILE_SUFFIXES_KEY,
    defaultValue = ErlangPlugin.FILE_SUFFIXES_DEFVALUE,
    name = "File suffixes",
    description = "Comma-separated list of suffixes for files to analyze. To not filter, leave the list empty.",
    global = true, project = true),

  @Property(key = ErlangPlugin.EUNIT_FOLDER_KEY,
    defaultValue = ErlangPlugin.EUNIT_DEFAULT_FOLDER,
    name = "Eunit Default Output Folder",
    description = "Folder where Eunit unit test and code coverage reports are located",
    global = true, project = true),

  @Property(key = ErlangPlugin.DIALYZER_FILENAME_KEY,
    defaultValue = ErlangPlugin.DIALYZER_DEFAULT_FILENAME,
    name = "Dialyzer Default Filename",
    description = "Filename of the dialyzer output located in the eunit folder",
    global = true, project = true),

})
public class ErlangPlugin extends SonarPlugin {

  public static final String EUNIT_FOLDER_KEY = "sonar.erlang.eunit.repotsfolder";
  public static final String EUNIT_DEFAULT_FOLDER = ".eunit/";
  public static final String DIALYZER_FILENAME_KEY = "sonar.erlang.dialyzer.filename";

  public static final String DIALYZER_DEFAULT_FILENAME = EUNIT_DEFAULT_FOLDER + "dialyzer.log";
  public static final String NAME = "Erlang";
  public static final String LANG_KEY = "erl";
  public static final String EXTENSION = "." + LANG_KEY;
  public static final String FILE_SUFFIXES_KEY = "sonar.erlang.file.suffixes";
  public static final String FILE_SUFFIXES_DEFVALUE = LANG_KEY;
  public static final String REBAR_CONFIG_URL = "rebar.config";

  public List<Class<? extends Extension>> getExtensions() {
    return ImmutableList.of(Erlang.class, ErlangSourceImporter.class,
        ErlangColorizerFormat.class, ErlangCpdMapping.class,

        ErlangSquidSensor.class,

        ErlangChecksRuleRepository.class, DialyzerRuleRepository.class,
        // ErlangChecksProfile.class,
        // DialyzerProfile.class,
        ErlangProfile.class,

        ErlangCommonRulesEngineProvider.class,

        EunitSensor.class,

        CoverCoverageSensor.class,

        DialyzerSensor.class);
  }

}
