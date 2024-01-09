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

import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.config.Configuration;

import java.io.File;

public class ErlangUtils {

    public static File findFile(SensorContext context, String fileName) {
        Configuration configuration = context.config();
        String basePath = context.fileSystem().baseDir().getPath();

        // Try to find it in the eunit folder first
        String eunitFolder = configuration.get(ErlangPlugin.EUNIT_FOLDER_KEY).orElse(ErlangPlugin.EUNIT_DEFAULT_FOLDER);
        File eunitReportsDir = new File(basePath, eunitFolder);
        File file = new File(eunitReportsDir, fileName);

        if (file.exists()) {
            return file;
        } else {
            // Try a path from the base directory
            return new File(basePath, fileName);
        }
    }

}
