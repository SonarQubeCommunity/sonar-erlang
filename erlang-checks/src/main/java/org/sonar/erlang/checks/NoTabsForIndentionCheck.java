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
package org.sonar.erlang.checks;

import com.sonar.sslr.api.AstNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.annotations.SqaleSubCharacteristic;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Scanner;

@Rule(key = "NoTabsForIndention", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleSubCharacteristic(RulesDefinition.SubCharacteristics.READABILITY)
@SqaleConstantRemediation("1min")
public class NoTabsForIndentionCheck extends SquidCheck<LexerlessGrammar> {

  private static final Logger LOG = LoggerFactory.getLogger(NoTabsForIndentionCheck.class);

  @Override
  public void visitFile(AstNode astNode) {
    File file = getContext().getFile();
    try {
      checkFileIndention(file);
    } catch (FileNotFoundException e) {
      LOG.error("File not found: " + file.getAbsolutePath(), e);
    }
  }

  private void checkFileIndention(File source) throws FileNotFoundException {
    Scanner scanner = new Scanner(new FileInputStream(source));
    int numOfViolations = 0;
    try {
      int lineNumber = 1;
      while (scanner.hasNextLine()) {
        String line = scanner.nextLine();

        if (line.matches("^ *\t+.*")) {
          getContext().createLineViolation(this, "Line has tabs as indention.",
            lineNumber);
          numOfViolations++;
        }
        if (numOfViolations == 100) {
          getContext().createLineViolation(this,
            "File has reached 100 'Line has tabs as indention' violation.",
            lineNumber);
          return;
        }
        lineNumber++;
      }
    } finally {
      scanner.close();
    }
  }

}
