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
package org.sonar.plugins.erlang;

import org.apache.commons.configuration.Configuration;
import org.mockito.Mockito;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issuable.IssueBuilder;
import org.sonar.api.issue.Issue;
import org.sonar.api.resources.InputFile;
import org.sonar.api.resources.InputFileUtils;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.ProjectFileSystem;
import org.sonar.api.resources.Resource;
import org.sonar.api.rule.RuleKey;
import org.sonar.api.scan.filesystem.ModuleFileSystem;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ProjectUtil {

  public static SensorContext mockContext() {
    SensorContext context = mock(SensorContext.class);
    when(context.isIndexed(any(Resource.class), eq(false))).thenReturn(true);
    return context;
  }

  public static Configuration mockConfiguration() {
    Configuration configuration = mock(Configuration.class);
    when(
        configuration.getString(ErlangPlugin.EUNIT_FOLDER_KEY,
            ErlangPlugin.EUNIT_DEFAULT_FOLDER)).thenReturn(
        ErlangPlugin.EUNIT_DEFAULT_FOLDER);
    when(
        configuration.getString(ErlangPlugin.DIALYZER_FILENAME_KEY,
            ErlangPlugin.DIALYZER_DEFAULT_FILENAME)).thenReturn(
        ErlangPlugin.DIALYZER_DEFAULT_FILENAME);
    return configuration;
  }

  public static Issuable mockIssueable() {
    Issuable issuable = mock(Issuable.class);
    final IssueBuilder issueBuilder = mock(Issuable.IssueBuilder.class);
    when(issuable.newIssueBuilder()).thenReturn(issueBuilder);
    when(issueBuilder.ruleKey(Mockito.any(RuleKey.class))).thenReturn(issueBuilder);
    when(issueBuilder.line(Mockito.any(Integer.class))).thenReturn(issueBuilder);
    when(issueBuilder.message(Mockito.any(String.class))).thenReturn(issueBuilder);

    Issue issue = mock(Issue.class);
    when(issueBuilder.build()).thenReturn(issue);
    return issuable;
  }

  public static ModuleFileSystem mockModuleFileSystem(List<File> srcFiles, List<File> testFiles) {
    ModuleFileSystem fileSystem = mock(ModuleFileSystem.class);
    when(fileSystem.sourceCharset()).thenReturn(Charset.forName("UTF-8"));
    when(fileSystem.baseDir()).thenReturn(new File("src/test/resources/org/sonar/plugins/erlang/erlcount/"));
    when(fileSystem.sourceDirs()).thenReturn(Arrays.asList(new File("src/test/resources/org/sonar/plugins/erlang/erlcount/src")));
    when(fileSystem.testDirs()).thenReturn(Arrays.asList(new File("src/test/resources/org/sonar/plugins/erlang/erlcount/test")));

    ArgumentMatchers m = new ArgumentMatchers();

    when(fileSystem.files(Mockito.argThat(m. new IsFileQuerySource()))).thenReturn(srcFiles);
    when(fileSystem.files(Mockito.argThat(m. new IsFileQuerySource()))).thenReturn(srcFiles);
    when(fileSystem.files(Mockito.argThat(m. new IsFileQueryTest()))).thenReturn(testFiles);
    when(fileSystem.files(Mockito.argThat(m. new IsFileQueryTest()))).thenReturn(testFiles);
    return fileSystem;
  }

  public static InputFile getInputFileByPath(String path) throws URISyntaxException {
    File fileToAnalyse = new File(ProjectUtil.class.getResource(path).toURI());
    InputFile inputFile = InputFileUtils.create(fileToAnalyse.getParentFile(), fileToAnalyse);
    return inputFile;
  }

  public static List<InputFile> getInputFiles(List<File> files) throws URISyntaxException {
    ArrayList<InputFile> ret = new ArrayList<InputFile>();
    for (File file : files) {
      ret.add(getInputFileByPath(file.getAbsolutePath()));
    }
    return ret;
  }

  /**
   * This is unavoidable in order to be compatible with sonarqube 4.2
   */
  public static void addProjectFileSystem(Project project, String srcDir) {
    ProjectFileSystem fs = mock(ProjectFileSystem.class);
    when(fs.getSourceDirs()).thenReturn(Arrays.asList(new File(srcDir)));

    project.setFileSystem(fs);
  }

}
