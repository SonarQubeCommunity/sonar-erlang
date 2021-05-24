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
package org.sonar.plugins.erlang;

import org.sonar.api.Plugin;
import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.plugins.erlang.checks.ErlangChecksRuleDefinition;
import org.sonar.plugins.erlang.cover.CoverCoverageSensor;
import org.sonar.plugins.erlang.dialyzer.DialyzerRuleDefinition;
import org.sonar.plugins.erlang.dialyzer.DialyzerSensor;
import org.sonar.plugins.erlang.eunit.EunitXmlSensor;
import org.sonar.plugins.erlang.languages.ErlangLanguage;
import org.sonar.plugins.erlang.settings.ErlangLanguageProperties;
import org.sonar.plugins.erlang.xref.XrefRuleDefinition;
import org.sonar.plugins.erlang.xref.XrefSensor;

@Properties({
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

    @Property(key = ErlangPlugin.XREF_FILENAME_KEY,
        defaultValue = ErlangPlugin.XREF_DEFAULT_FILENAME,
        name = "Xref Default Filename",
        description = "Filename of the xref output located in the eunit folder",
        global = true, project = true),

    @Property(key = ErlangPlugin.EUNIT_COVERDATA_FILENAME_KEY,
        defaultValue = ErlangPlugin.EUNIT_COVERDATA_DEFAULT_FILENAME,
        name = "EUnit coverdata Default Filename",
        description = "Filename of the Eunit generated coverdata file located in the eunit folder",
        global = true, project = true),

    @Property(key = ErlangPlugin.CT_COVERDATA_FILENAME_KEY,
        defaultValue = ErlangPlugin.CT_COVERDATA_DEFAULT_FILENAME,
        name = "CommonTest coverdata default file name.",
        description = "Path to the Common Test coverage data file.",
        global = true, project = true),

    @Property(key = ErlangPlugin.REBAR_CONFIG_FILENAME_KEY,
        defaultValue = ErlangPlugin.REBAR_DEFAULT_CONFIG_FILENAME,
        name = "Rebar configfile name",
        description = "Filename of the rebar config file",
        global = true, project = true)
})
public class ErlangPlugin implements Plugin {

  public static final String EUNIT_FOLDER_KEY = "sonar.erlang.eunit.reportsfolder";
  public static final String EUNIT_DEFAULT_FOLDER = ".eunit/";

  public static final String EUNIT_COVERDATA_FILENAME_KEY = "sonar.erlang.eunit.coverdata.filename";
  public static final String EUNIT_COVERDATA_DEFAULT_FILENAME = "eunit.coverdata";

  public static final String CT_COVERDATA_FILENAME_KEY = "sonar.erlang.ct.coverdata.filename";
  public static final String CT_COVERDATA_DEFAULT_FILENAME = "all.coverdata";

  public static final String DIALYZER_FILENAME_KEY = "sonar.erlang.dialyzer.filename";
  public static final String DIALYZER_DEFAULT_FILENAME = "dialyzer.log";

  public static final String XREF_FILENAME_KEY = "sonar.erlang.xref.filename";
  public static final String XREF_DEFAULT_FILENAME = "xref.log";

  public static final String NAME = "Erlang";
  public static final String REBAR_CONFIG_FILENAME_KEY = "sonar.erlang.rebar.config";
  public static final String REBAR_DEFAULT_CONFIG_FILENAME = "rebar.config";

  @Override
  public void define(Context context) {
    // languages
    context.addExtension(ErlangLanguage.class);
    context.addExtension(ErlangLanguageProperties.getProperties());

    context.addExtensions(
        ErlangHighlighter.class,
        ErlangCpdVisitor.class,

        ErlangSquidSensor.class,

        ErlangChecksRuleDefinition.class,
        DialyzerRuleDefinition.class,
        XrefRuleDefinition.class,
        ErlangProfile.class,

        EunitXmlSensor.class,

        CoverCoverageSensor.class,

        DialyzerSensor.class,
        XrefSensor.class
    );
  }
}
