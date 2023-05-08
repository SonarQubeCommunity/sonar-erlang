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
package org.sonar.plugins.erlang.elvis;

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
import org.sonar.plugins.erlang.ErlangUtils;
import org.sonar.plugins.erlang.xml.XmlRuleManager;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Read and parse generated elvis report
 */
public class ElvisReportParser {

  private static final String ELVIS_VIOLATION_ROW_REGEX = "([^ :]*):([^ :]*):([^ :]*):(.*)";
  private static final String REPO_KEY = ElvisRuleDefinition.REPOSITORY_KEY;
  private static final Logger LOG = Loggers.get(ElvisReportParser.class);
  private final SensorContext context;
  private final Configuration configuration;

  ElvisReportParser(SensorContext context) {
    this.context = context;
    this.configuration = context.config();
  }

  /**
   * Parse elvis report results using a set of rules
   *
   * @param ruleManager set of Erlang rules
   */
  public void parse(XmlRuleManager ruleManager) {
    String reportFileName = configuration.get(ErlangPlugin.ELVIS_FILENAME_KEY).orElse(ErlangPlugin.ELVIS_DEFAULT_FILENAME);
    File elvisReportFile = ErlangUtils.findFile(context, reportFileName);

    if (elvisReportFile.exists()) {
      try {
        parseElvisLogFile(context, elvisReportFile, ruleManager);
      } catch (FileNotFoundException e) {
        LOG.warn("Elvis file not found at: {} have you ran elvis before analysis?", reportFileName, e);
      } catch (IOException e) {
        LOG.error("Error while trying to parse elvis report at {}.", reportFileName, e);
      }
    } else {
      LOG.warn("Could not find Elvis report file at: {} , skipping...", elvisReportFile.getPath());
    }
  }

  /**
   * Get the elvis report target directory key.
   *
   * @return repository key
   */
  public static String getRepoKey() {
    return REPO_KEY;
  }

  /**
   * Get current elvis report parser configuration
   *
   * @return configuration
   */
  public Configuration getConfiguration() {
    return this.configuration;
  }

  private void parseElvisLogFile(SensorContext context, File elvisLogFile, XmlRuleManager elvisRuleManager) throws IOException {
    InputStream fstream = Files.newInputStream(elvisLogFile.toPath());
    DataInputStream in = new DataInputStream(fstream);

    try (BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))) {
      String strLine;
      Pattern pattern = Pattern.compile(ELVIS_VIOLATION_ROW_REGEX);

      while ((strLine = bufferedReader.readLine()) != null) {
        Matcher matcher = pattern.matcher(strLine);

        if (!matcher.matches()) continue;

        String fileName = matcher.group(1);
        String lineNumber = matcher.group(2);
        String ruleName = matcher.group(3);
        String comment = matcher.group(4).trim();

        String key = elvisRuleManager.getRuleKeyByMessage(ruleName);

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
    } catch (FileNotFoundException e) {
      LOG.warn("Elvis file not found at: {}, have you ran elvis before analysis?", elvisLogFile.getPath());
    } catch (IOException e) {
      LOG.error("Error while trying to parse elvis report at: {}", elvisLogFile.getPath(), e);
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
