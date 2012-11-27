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

import org.sonar.erlang.checks.CommentRegularExpressionCheck;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;
import org.junit.Test;
import org.sonar.squid.api.SourceFile;

import java.io.File;

public class CommentRegularExpressionCheckTest {

  @Test
  public void test() {
    CommentRegularExpressionCheck check = new CommentRegularExpressionCheck();

    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/checks/commentcheck.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages()).noMore();
  }

  @Test
  public void testDifferentValue() {
    CommentRegularExpressionCheck check = new CommentRegularExpressionCheck();
    check.regularExpression = "(?i).*TODO.*";
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/checks/commentcheck.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(2).withMessage(
        "The regular expression matches this comment").next().atLine(9);
  }

  @Test
  public void testDifferentValue2() {
    CommentRegularExpressionCheck check = new CommentRegularExpressionCheck();
    check.regularExpression = "(?i).*XXX.*";
    check.message = "XXX found";
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/checks/commentcheck.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(8).withMessage(
        "XXX found").noMore();
  }

}
