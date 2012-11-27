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

import org.sonar.erlang.ErlangAstScanner;

import org.sonar.erlang.checks.FunctionDefAndClausesSeparationCheck;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;
import org.junit.Test;
import org.sonar.squid.api.SourceFile;

import java.io.File;

public class FunctionDefAndClausesSeparationCheckTest {

  @Test
  public void test() {
    FunctionDefAndClausesSeparationCheck check = new FunctionDefAndClausesSeparationCheck();

    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/checks/functionseparation.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(12).withMessage(
        "The line has 0 precending blank line and it should be: 1.").next().atLine(22)
        .withMessage("The line has 1 precending blank line and it should be: 0.").next()
        .atLine(26)
        .withMessage("The line has 2 precending blank line and it should be: 1.").noMore();
  }
}
