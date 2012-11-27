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

import org.sonar.erlang.checks.SpaceAfterBeforeOperatorsCheck;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;
import org.junit.Test;
import org.sonar.squid.api.SourceFile;

import java.io.File;

public class SpaceAfterBeforeOperatorsCheckTest {

  @Test
  public void test() {
    SpaceAfterBeforeOperatorsCheck check = new SpaceAfterBeforeOperatorsCheck();

    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/checks/spaceafteroperator.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(4).withMessage(
        "No space after operator in column: 9.").next().atLine(5).withMessage(
        "No space after operator in column: 12.").next().atLine(6).withMessage(
        "No space after operator in column: 5.").next().atLine(7).withMessage(
        "No space after operator in column: 8.").next().atLine(9).withMessage(
        "No space after operator in column: 13.").noMore();
  }
}
