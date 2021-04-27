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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.internal.MapSettings;
import org.sonar.plugins.erlang.ErlangPlugin;

import java.io.File;

public class XrefReportParserTest {

  private SensorContext sensorContext;
  private final File testModuleBasedir = new File("src/test/resources/org/sonar/plugins/erlang/erlcount/");

  @Before
  public void setUp() {
    MapSettings settings = new MapSettings(new PropertyDefinitions(ErlangPlugin.class));
    settings.setProperty(ErlangPlugin.EUNIT_FOLDER_KEY, "sonar.erlang.eunit.reportsfolder");
    settings.setProperty(ErlangPlugin.XREF_FILENAME_KEY, "sonar.erlang.xref.filename");
    sensorContext = SensorContextTester.create(testModuleBasedir).setSettings(settings);
  }

  @Test
  public void testGetRepoKey() {
    Assert.assertEquals("xref", XrefReportParser.getRepoKey());
  }

  @Test
  public void testGetConfiguration() {
    XrefReportParser parser = new XrefReportParser(sensorContext);

    Assert.assertTrue(
            parser.getConfiguration().get(ErlangPlugin.EUNIT_FOLDER_KEY).isPresent()
    );

    Assert.assertTrue(
            parser.getConfiguration().get(ErlangPlugin.XREF_FILENAME_KEY).isPresent()
    );
  }

}
