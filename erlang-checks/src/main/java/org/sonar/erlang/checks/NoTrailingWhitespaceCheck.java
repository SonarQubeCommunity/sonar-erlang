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
package org.sonar.erlang.checks;

import org.sonar.erlang.api.ErlangGrammar;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Scanner;

@Rule(key = "NoTrailingWhiteSpace", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE,
  name = "NoTrailingWhiteSpace", description = "No trailing white space is allowed")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NoTrailingWhitespaceCheck extends SquidCheck<ErlangGrammar> {

  @Override
  public void visitFile(AstNode astNode) {
    try {
      checkFileIndention(getContext().getFile());
    } catch (FileNotFoundException e) {
    }
  }

  @Override
  public void leaveFile(AstNode astNode) {
  }

  private void checkFileIndention(File source) throws FileNotFoundException {
    Scanner scanner = new Scanner(new FileInputStream(source));
    try {
      int lineNumber = 1;
      while (scanner.hasNextLine()) {
        String line = scanner.nextLine();
        if (line.matches(".*\\s+$")) {
          getContext().createLineViolation(this, "No trailing white space.", lineNumber);
        }
        lineNumber++;
      }
    } finally {
      scanner.close();
    }
  }
}
