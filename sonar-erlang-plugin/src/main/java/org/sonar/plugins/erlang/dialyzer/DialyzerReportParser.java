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
package org.sonar.plugins.erlang.dialyzer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.config.Settings;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.api.rule.RuleKey;
import org.sonar.api.scan.filesystem.ModuleFileSystem;
import org.sonar.plugins.erlang.ErlangPlugin;

import java.io.*;

/**
 * Read and parse generated dialyzer report
 *
 * @author tkende
 */
public class DialyzerReportParser {
  private static final String DIALYZER_VIOLATION_ROW_REGEX = "(.*?)(:[0-9]+:)(.*)";
  private static final String REPO_KEY = DialyzerRuleRepository.REPOSITORY_KEY;
  private static final Logger LOG = LoggerFactory.getLogger(DialyzerReportParser.class);

  private ModuleFileSystem moduleFileSystem;
  private ResourcePerspectives resourcePerspectives;

  public DialyzerReportParser(ModuleFileSystem moduleFileSystem, ResourcePerspectives resourcePerspectives) {
    this.moduleFileSystem = moduleFileSystem;
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
      File reportsDir = new File(moduleFileSystem.baseDir(), settings.getString(ErlangPlugin.EUNIT_FOLDER_KEY));

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
          String ruleKey = dialyzerRuleManager.getRuleKeyByMessage(res[2].trim());
          if (rulesProfile.getActiveRule(REPO_KEY, ruleKey) != null) {
            org.sonar.api.resources.File resource = getResourceByFileName(res[0], project);
            if (resource != null) {
              Issuable issuable = resourcePerspectives.as(Issuable.class, resource);
              Issue issue = issuable.newIssueBuilder()
                .ruleKey(RuleKey.of(REPO_KEY, ruleKey))
                .line(Integer.valueOf(res[1]))
                .message(res[2].trim())
                .build();
              issuable.addIssue(issue);
            }
          }
        }
      }
      breader.close();
    } catch (FileNotFoundException e) {
      LOG.error("Dialyser file not found: " + dialyzerFileName, e);
    } catch (IOException e) {
      LOG.error("Error while trying to read the file: " + dialyzerFileName, e);
    }
  }

  protected org.sonar.api.resources.File getResourceByFileName(String fileName, Project project) {
    for (File sourceDir : moduleFileSystem.sourceDirs()) {
      File file = new File(sourceDir, fileName);
      if (file.exists()) {
        return org.sonar.api.resources.File.fromIOFile(file, project);
      }
    }
    return null;
  }

}
