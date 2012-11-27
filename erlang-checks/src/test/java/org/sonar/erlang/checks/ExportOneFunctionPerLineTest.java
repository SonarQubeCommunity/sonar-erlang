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

import org.sonar.erlang.checks.ExportOneFunctionPerLineCheck;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;
import org.junit.Test;
import org.sonar.squid.api.SourceFile;

import java.io.File;

public class ExportOneFunctionPerLineTest {

  @Test
  public void test() {
    ExportOneFunctionPerLineCheck check = new ExportOneFunctionPerLineCheck();

    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/checks/export_one_function_per_line.erl"), check);
    CheckMessagesVerifier
        .verify(file.getCheckMessages())
        .next()
        .atLine(3)
        .withMessage(
            "The exported method with arity: kill/0 is in the same line, but it has different name than the previous arity: stop/0.")
        .next()
        .atLine(5)
        .withMessage(
            "The exported method with arity: log/3 is in different line, but it has the same name as the previous arity: log/2.")
        .next()
        .atLine(7)
        .withMessage(
            "The exported method with arity: refresh/2 is in the same line, but it has different name than the previous arity: update/2.")
        .noMore();
  }
}
