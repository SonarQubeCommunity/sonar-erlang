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
package org.sonar.plugins.erlang;

import java.io.File;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.List;

import org.mockito.Mockito;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultFileSystem;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.api.batch.fs.internal.TestInputFileBuilder;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issuable.IssueBuilder;
import org.sonar.api.issue.Issue;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.erlang.core.Erlang;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ProjectUtil {

  public static SensorContext mockContext() {
    return mock(SensorContext.class);
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

  public static FileSystem createFileSystem(String baseDir, List<String> srcFiles, List<String> testFiles) throws Exception{
    DefaultFileSystem fileSystem = new DefaultFileSystem(new File(baseDir));

    fileSystem.setEncoding(Charset.forName("UTF-8"));

    if (srcFiles != null) {
      for (String srcFile : srcFiles) {
        addFile(fileSystem, srcFile, InputFile.Type.MAIN);
      }
    }

    if (testFiles != null) {
      for (String testFile : testFiles) {
        addFile(fileSystem, testFile, InputFile.Type.TEST);
      }
    }

    return fileSystem;
  }

  private static void addFile(DefaultFileSystem fileSystem, String file, InputFile.Type type) throws Exception{

    File testModuleBasedir = new File("src/test/resources/");
    DefaultInputFile dif = new TestInputFileBuilder("key", file)
            .setLanguage(Erlang.KEY).setType(type)
            .setModuleBaseDir(testModuleBasedir.toPath())
            .initMetadata(new String(Files.readAllBytes(testModuleBasedir.toPath().resolve(file))))
            .build();

    fileSystem.add(dif);
  }

}
