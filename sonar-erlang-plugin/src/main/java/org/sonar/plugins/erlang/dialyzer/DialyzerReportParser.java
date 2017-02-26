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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.TextRange;
import org.sonar.api.batch.rule.ActiveRule;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.issue.NewIssue;
import org.sonar.api.batch.sensor.issue.NewIssueLocation;
import org.sonar.api.config.Settings;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.erlang.ErlangPlugin;

import java.io.*;

/**
 * Read and parse generated dialyzer report
 *
 * @author tkende
 */
public class DialyzerReportParser {

  private static final String DIALYZER_VIOLATION_ROW_REGEX = "(.*?)(:[0-9]+:)(.*)";
  private static final String REPO_KEY = DialyzerRuleDefinition.REPOSITORY_KEY;
  private static final Logger LOG = LoggerFactory.getLogger(DialyzerReportParser.class);
  private final SensorContext context;


  DialyzerReportParser(SensorContext context) {
    this.context = context;
  }

  public void dialyzer(ErlangRuleManager dialyzerRuleManager) {
    /*
      Read dialyzer results
     */
    Settings settings = context.settings();
    String dialyzerFileName = null;

    try {
      File reportsDir = new File(context.fileSystem().baseDir().getPath(),
              settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY));

      dialyzerFileName = settings.getString(ErlangPlugin.DIALYZER_FILENAME_KEY);
      File file = new File(reportsDir, dialyzerFileName);

      FileInputStream fstream = new FileInputStream(file);
      DataInputStream in = new DataInputStream(fstream);
      BufferedReader dialyzerOutput = new BufferedReader(new InputStreamReader(in));
      BufferedReader breader = new BufferedReader(dialyzerOutput);

      String strLine;
      while ((strLine = breader.readLine()) != null) {
        if (strLine.matches(DIALYZER_VIOLATION_ROW_REGEX)) {
          String[] res = strLine.split(":");
          String fileName = res[0];
          String line = res[1];
          String message = res[2].trim();

          String key = dialyzerRuleManager.getRuleKeyByMessage(message);
          RuleKey ruleKey = RuleKey.of(REPO_KEY, key);
          ActiveRule rule = context.activeRules().find(ruleKey);
          if (rule != null) {
            String filePattern = "**/"+fileName;
            InputFile inputFile = context.fileSystem().inputFile(
                    context.fileSystem().predicates().matchesPathPattern(filePattern));
            if (inputFile != null) {
              NewIssue issue = getNewIssue(line, message, ruleKey, inputFile);
              issue.save();
            }
          }
        }
      }
      breader.close();
    } catch (FileNotFoundException e) {
      LOG.warn("Dialyser file not found at: " + dialyzerFileName + ", have you ran dialyzer before analysis?", e);
    } catch (IOException e) {
      LOG.error("Error while trying to parse dialyzer report at: " + dialyzerFileName, e);
    }
  }

  private NewIssue getNewIssue(String line, String message, RuleKey ruleKey, InputFile inputFile) {
    TextRange range = inputFile.selectLine(Integer.valueOf(line));

    NewIssue issue = context
            .newIssue()
            .forRule(ruleKey);

    NewIssueLocation location = issue.newLocation()
            .on(inputFile)
            .at(range)
            .message(message);

    issue.at(location);
    return issue;
  }
}
