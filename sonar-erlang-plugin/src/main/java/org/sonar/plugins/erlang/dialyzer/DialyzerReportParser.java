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

import org.sonar.api.batch.SensorContext;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Read and parse generated dialyzer report
 * 
 * @author tkende
 * 
 */
public class DialyzerReportParser {
  private static final String DIALYZER_VIOLATION_ROW_REGEX = "(.*?)(:[0-9]+:)(.*)";
  private static final String REPO_KEY = DialyzerRuleRepository.REPOSITORY_KEY;

  /**
   * We must pass the dialyzerRuleManager as well to make possible to find the
   * rule based on the message in the dialyzer log file
   * 
   * @param project
   * @param context
   * @param dialyzerRuleManager
   * @param rulesProfile
   * @return
   */
  public void dialyzer(Project project, SensorContext context,
      ErlangRuleManager dialyzerRuleManager, RulesProfile rulesProfile) {
    /**
     * Read dialyzer results
     */
    try {
      String dialyzerUri = ((Erlang) project.getLanguage()).getConfiguration().getString(
          ErlangPlugin.DIALYZER_FILENAME_KEY, ErlangPlugin.DIALYZER_DEFAULT_FILENAME);
      File file = new File(project.getFileSystem().getBasedir(), dialyzerUri);
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
            org.sonar.api.resources.File resource = getResourceByFileName(project,
                res[0]);
            if (resource != null) {
              Rule rule = Rule.create(REPO_KEY, ruleKey);
              Violation violation = Violation.create(rule, resource);
              violation.setLineId(Integer.valueOf(res[1]));
              violation.setMessage(res[2].trim());
              context.saveViolation(violation);
            }
          }
        }
      }
      breader.close();
    } catch (FileNotFoundException e) {
    } catch (IOException e) {
    }
  }

  protected org.sonar.api.resources.File getResourceByFileName(Project project, String fileName) {
    for (File sourceDir : project.getFileSystem().getSourceDirs()) {
      File file = new File(sourceDir, fileName);
      if (file.exists()) {
        return org.sonar.api.resources.File.fromIOFile(file, project);
      }
    }
    return null;
  }

}
