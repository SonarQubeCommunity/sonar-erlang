/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2018 Tamas Kende; Denes Hegedus (Cursor Insight Ltd.)
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
package org.sonar.plugins.erlang.xref;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.rule.ActiveRule;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.issue.NewIssue;
import org.sonar.api.batch.sensor.issue.NewIssueLocation;
import org.sonar.api.config.Settings;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.dialyzer.ErlangRuleManager;

import java.io.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Read and parse generated xref report
 */
public class XrefReportParser {

  private static final String XREF_VIOLATION_ROW_REGEX = "Warning: ([^ :]*):([^/]*)/([0-9]+) .*";
  private static final String REPO_KEY = XrefRuleDefinition.REPOSITORY_KEY;
  private static final Logger LOG = LoggerFactory.getLogger(XrefReportParser.class);
  private final SensorContext context;


  XrefReportParser(SensorContext context) {
    this.context = context;
  }

  public void xref(ErlangRuleManager ruleManager) {
    /*
      Read xref results
     */
    Settings settings = context.settings();
    String reportFileName = null;

    try {
      File reportsDir = new File(context.fileSystem().baseDir().getPath(),
              settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY));

      reportFileName = settings.getString(ErlangPlugin.XREF_FILENAME_KEY);
      File file = new File(reportsDir, reportFileName);

      FileInputStream fstream = new FileInputStream(file);
      DataInputStream in = new DataInputStream(fstream);
      BufferedReader xrefOutput = new BufferedReader(new InputStreamReader(in));
      BufferedReader breader = new BufferedReader(xrefOutput);

      String strLine;
      Pattern pattern = Pattern.compile(XREF_VIOLATION_ROW_REGEX);
      while ((strLine = breader.readLine()) != null) {
        Matcher matcher = pattern.matcher(strLine);
        if (!matcher.matches()){
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
      breader.close();
    } catch (FileNotFoundException e) {
      LOG.warn("Xref file not found at: " + reportFileName + ", have you ran xref before analysis?", e);
    } catch (IOException e) {
      LOG.error("Error while trying to parse xref report at: " + reportFileName, e);
    }
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
