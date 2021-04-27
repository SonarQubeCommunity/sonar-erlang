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

import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.rule.ActiveRule;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.issue.NewIssue;
import org.sonar.api.batch.sensor.issue.NewIssueLocation;
import org.sonar.api.config.Configuration;
import org.sonar.api.rule.RuleKey;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.dialyzer.ErlangRuleManager;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Read and parse generated xref report
 */
public class XrefReportParser {

  private static final String XREF_VIOLATION_ROW_REGEX = "Warning: ([^ :]*):([^/]*)/([0-9]+) .*";
  private static final String REPO_KEY = XrefRuleDefinition.REPOSITORY_KEY;
  private static final Logger LOG = Loggers.get(XrefReportParser.class);
  private final SensorContext context;
  private final Configuration configuration;

  XrefReportParser(SensorContext context) {

    this.context = context;
    this.configuration = context.config();
  }

  /**
   * Parse xref report results using a set of rules
   *
   * @param ruleManager set of Erlang rules
   */
  public void xref(ErlangRuleManager ruleManager) {
    String reportFileName = ErlangPlugin.XREF_DEFAULT_FILENAME;
    try {
      File reportsDir = new File(context.fileSystem().baseDir().getPath(),
              configuration.get(ErlangPlugin.EUNIT_FOLDER_KEY).orElse(ErlangPlugin.EUNIT_DEFAULT_FOLDER));

      reportFileName = configuration.get(ErlangPlugin.XREF_FILENAME_KEY).orElse(ErlangPlugin.XREF_DEFAULT_FILENAME);

      File file = new File(reportsDir, reportFileName);
      DataInputStream in = new DataInputStream(Files.newInputStream(file.toPath()));

      try (BufferedReader breader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))) {

        String strLine;
        Pattern pattern = Pattern.compile(XREF_VIOLATION_ROW_REGEX);
        while ((strLine = breader.readLine()) != null) {
          Matcher matcher = pattern.matcher(strLine);
          if (!matcher.matches()) {
            continue;
          }

          String fileName = matcher.group(1);

          String key = ruleManager.getRuleKeyByMessage(strLine);
          RuleKey ruleKey = RuleKey.of(REPO_KEY, key);
          ActiveRule rule = context.activeRules().find(ruleKey);
          if (rule != null) {
            String filePattern = "**/" + fileName + ".erl";
            InputFile inputFile = context.fileSystem().inputFile(
                    context.fileSystem().predicates().matchesPathPattern(filePattern));
            if (inputFile != null) {
              NewIssue issue = getNewIssue(strLine, ruleKey, inputFile);
              issue.save();
            }
          }
        }
      }
    } catch (FileNotFoundException e) {
      LOG.warn("Xref file not found at: {} have you ran xref before analysis?", reportFileName, e);
    } catch (IOException e) {
      LOG.error("Error while trying to parse xref report at {}.", reportFileName, e);
    }
  }

  /**
   * Get the xref report target directory key.
   *
   * @return repository key
   */
  public static String getRepoKey() {
    return REPO_KEY;
  }

  /**
   * Get current xref report parser configuration
   *
   * @return configuration
   */
  public Configuration getConfiguration() {
    return this.configuration;
  }

  private NewIssue getNewIssue(String message, RuleKey ruleKey, InputFile inputFile) {
    NewIssue issue = context.newIssue().forRule(ruleKey);
    NewIssueLocation location = issue.newLocation()
            .on(inputFile)
            .message(message);

    issue.at(location);
    return issue;
  }
}
