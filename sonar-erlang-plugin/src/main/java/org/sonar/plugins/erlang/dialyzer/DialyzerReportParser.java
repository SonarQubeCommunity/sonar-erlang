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
package org.sonar.plugins.erlang.dialyzer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FilePredicates;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.config.Settings;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.*;
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
  private static final Logger LOG = LoggerFactory.getLogger(DialyzerReportParser.class);

  private FileSystem fileSystem;
  private ResourcePerspectives resourcePerspectives;

  public DialyzerReportParser(FileSystem fileSystem, ResourcePerspectives resourcePerspectives) {
    this.fileSystem = fileSystem;
    this.resourcePerspectives = resourcePerspectives;
  }

  /**
   * We must pass the dialyzerRuleManager as well to make possible to find the
   * rule based on the message in the dialyzer log file
   *
   * @param settings
   * @param project
   * @param context
   * @param dialyzerRuleManager
   * @param rulesProfile
   * @return
   */
  public void dialyzer(Settings settings, SensorContext context, ErlangRuleManager dialyzerRuleManager, RulesProfile rulesProfile, Project project) {
    /**
     * Read dialyzer results
     */
    String dialyzerFileName = null;

    try {
      File reportsDir = new File(fileSystem.baseDir(), settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY));

      dialyzerFileName = settings.getString(ErlangPlugin.DIALYZER_FILENAME_KEY);
      File file = new File(reportsDir, dialyzerFileName);

      FileInputStream fstream = new FileInputStream(file);
      DataInputStream in = new DataInputStream(fstream);
      BufferedReader dialyzerOutput = new BufferedReader(new InputStreamReader(in));
      BufferedReader breader = new BufferedReader(dialyzerOutput);

      String strLine;
      Pattern pattern = Pattern.compile(DIALYZER_VIOLATION_ROW_REGEX);
      while ((strLine = breader.readLine()) != null) {
        Matcher matcher = pattern.matcher(strLine);
        if (!matcher.matches()) continue;
        String filename = matcher.group(1);
        String lineNumber = matcher.group(2);
        String comment = matcher.group(3).trim();
        String ruleKey = dialyzerRuleManager.getRuleKeyByMessage(comment);
        if (rulesProfile.getActiveRule(REPO_KEY, ruleKey) == null) continue;
        LOG.debug("Getting resource from path: " + filename);
        Issuable issuable = getResourceByFileName(filename);
        LOG.debug("Issuable is: " + issuable);
        if (issuable == null) continue;
        Issuable.IssueBuilder builder = issuable.newIssueBuilder();
        LOG.debug("Issuable builder is: " + builder);
        if (builder == null) continue;
        Issue issue = builder
                .ruleKey(RuleKey.of(REPO_KEY, ruleKey))
                .line(Integer.valueOf(lineNumber))
                .message(comment)
                .build();
        issuable.addIssue(issue);
      }
      breader.close();
    } catch (FileNotFoundException e) {
      LOG.warn("Dialyser file not found at: " + dialyzerFileName + ", have you ran dialyzer before analysis?", e);
    } catch (IOException e) {
      LOG.error("Error while trying to parse dialyzer report at: " + dialyzerFileName, e);
    }
  }

  protected Issuable getResourceByFileName(String fileName) {
    FilePredicates p = fileSystem.predicates();
    FilePredicate predicate = p.and(p.hasType(InputFile.Type.MAIN), p.hasLanguage(Erlang.KEY));
    for (File file : fileSystem.files(predicate)) {
      InputFile erlFile = fileSystem.inputFile(fileSystem.predicates().is(file));
      if (!file.getAbsolutePath().endsWith(fileName)) {
        continue;
      }
      Issuable issuable = resourcePerspectives.as(Issuable.class, erlFile);
      if(issuable != null) {
        return issuable;
      }
    }

    return null;
  }

}
