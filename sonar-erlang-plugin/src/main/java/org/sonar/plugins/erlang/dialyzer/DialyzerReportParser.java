/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
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
package org.sonar.plugins.erlang.dialyzer;

import org.apache.commons.io.FilenameUtils;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.TextRange;
import org.sonar.api.batch.rule.ActiveRule;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.issue.NewIssue;
import org.sonar.api.batch.sensor.issue.NewIssueLocation;
import org.sonar.api.config.Configuration;
import org.sonar.api.rule.RuleKey;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonar.plugins.erlang.ErlangPlugin;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Read and parse generated dialyzer report
 *
 * @author tkende
 */
public class DialyzerReportParser {

  private static final String DIALYZER_VIOLATION_ROW_REGEX = "(.*?):([0-9]+):(.*)";
  private static final String REPO_KEY = DialyzerRuleDefinition.REPOSITORY_KEY;
  private static final Logger LOG = Loggers.get(DialyzerReportParser.class);
  private final SensorContext context;


  DialyzerReportParser(SensorContext context) {
    this.context = context;
  }

  public void dialyzer(ErlangRuleManager dialyzerRuleManager) {
    /*
      Read dialyzer results
     */
    Configuration configuration = context.config();
    String dialyzerFileName = null;

    try {
      File reportsDir = new File(context.fileSystem().baseDir().getPath(),
              configuration.get(ErlangPlugin.EUNIT_FOLDER_KEY).orElse(ErlangPlugin.EUNIT_DEFAULT_FOLDER));

      dialyzerFileName = configuration.get(ErlangPlugin.DIALYZER_FILENAME_KEY).orElse(ErlangPlugin.DIALYZER_DEFAULT_FILENAME);
      File file = new File(reportsDir, dialyzerFileName);

      InputStream fstream = Files.newInputStream(file.toPath());
      DataInputStream in = new DataInputStream(fstream);
      try (BufferedReader breader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))) {

        String strLine;
        Pattern pattern = Pattern.compile(DIALYZER_VIOLATION_ROW_REGEX);
        while ((strLine = breader.readLine()) != null) {
          Matcher matcher = pattern.matcher(strLine);
          if (!matcher.matches()) {
            continue;
          }

          String fileName = matcher.group(1);
          String lineNumber = matcher.group(2);
          String comment = matcher.group(3).trim();

          String key = dialyzerRuleManager.getRuleKeyByMessage(comment);
          RuleKey ruleKey = RuleKey.of(REPO_KEY, key);
          ActiveRule rule = context.activeRules().find(ruleKey);
          if (rule != null) {
            String filePattern = "**/" + FilenameUtils.getName(fileName);
            InputFile inputFile = context.fileSystem().inputFile(
                    context.fileSystem().predicates().matchesPathPattern(filePattern));
            if (inputFile != null) {
              NewIssue issue = getNewIssue(lineNumber, comment, ruleKey, inputFile);
              issue.save();
            }
          }
        }
      }
    } catch (FileNotFoundException e) {
      LOG.warn("Dialyzer file not found at: {}, have you ran dialyzer before analysis?", dialyzerFileName);
    } catch (IOException e) {
      LOG.error("Error while trying to parse dialyzer report at: {}", dialyzerFileName, e);
    }
  }

  private NewIssue getNewIssue(String line, String message, RuleKey ruleKey, InputFile inputFile) {
    TextRange range = inputFile.selectLine(Integer.parseInt(line));
    NewIssue issue = context.newIssue().forRule(ruleKey);
    NewIssueLocation location = issue.newLocation()
            .on(inputFile)
            .at(range)
            .message(message);

    issue.at(location);
    return issue;
  }
}
