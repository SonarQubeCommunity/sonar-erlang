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

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;
import org.junit.Test;
import org.sonar.squid.api.SourceFile;

import java.io.File;

public class NoMacrosTest {

  @Test
  public void test() {
    NoMacrosCheck check = new NoMacrosCheck();

    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(9).withMessage("Do not use macros.")
      .noMore();
  }

  @Test
  public void test2() {
    NoMacrosCheck check = new NoMacrosCheck();
    check.setSkipDefineInFlowControl(false);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(4).withMessage("Do not use macros.")
      .next().atLine(6).withMessage("Do not use macros.")
      .next().atLine(7).withMessage("Do not use macros.")
      .next().atLine(9).withMessage("Do not use macros.")
      .noMore();
  }

  @Test
  public void test3() {
    NoMacrosCheck check = new NoMacrosCheck();
    check.setAllowLiteralMacros(false);
    check.setIgnoredMacroNames("");
    check.setSkipDefineInFlowControl(false);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(2).withMessage("Do not use macros.")
      .next().atLine(4).withMessage("Do not use macros.")
      .next().atLine(6).withMessage("Do not use macros.")
      .next().atLine(7).withMessage("Do not use macros.")
      .next().atLine(9).withMessage("Do not use macros.")
      .next().atLine(13).withMessage("Do not use macros.").noMore();
  }

  @Test
  public void test3_1() {
    NoMacrosCheck check = new NoMacrosCheck();
    check.setAllowLiteralMacros(false);
    check.setIgnoredMacroNames("");
    check.setSkipDefineInFlowControl(true);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(2).withMessage("Do not use macros.")
      .next().atLine(9).withMessage("Do not use macros.")
      .next().atLine(13).withMessage("Do not use macros.").noMore();
  }

  @Test
  public void test3_2() {
    NoMacrosCheck check = new NoMacrosCheck();
    check.setAllowLiteralMacros(true);
    check.setIgnoredMacroNames("");
    check.setSkipDefineInFlowControl(false);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(4).withMessage("Do not use macros.")
      .next().atLine(6).withMessage("Do not use macros.")
      .next().atLine(7).withMessage("Do not use macros.")
      .next().atLine(9).withMessage("Do not use macros.")
      .noMore();
  }

  @Test
  public void test3_3() {
    NoMacrosCheck check = new NoMacrosCheck();
    check.setAllowLiteralMacros(true);
    check.setIgnoredMacroNames("");
    check.setSkipDefineInFlowControl(true);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(9).withMessage("Do not use macros.")
      .noMore();
  }

  @Test
  public void test3_4() {
    NoMacrosCheck check = new NoMacrosCheck();
    check.setAllowLiteralMacros(false);
    check.setIgnoredMacroNames("IGNOREME,B");
    check.setSkipDefineInFlowControl(false);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(2).withMessage("Do not use macros.")
      .next().atLine(4).withMessage("Do not use macros.")
      .next().atLine(6).withMessage("Do not use macros.")
      .noMore();
  }

  public void test3_5() {
    NoMacrosCheck check = new NoMacrosCheck();
    check.setAllowLiteralMacros(true);
    check.setIgnoredMacroNames("IGNOREME,B");
    check.setSkipDefineInFlowControl(false);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/nomacros2.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(4).withMessage("Do not use macros.")
      .next().atLine(6).withMessage("Do not use macros.")
      .noMore();
  }
}
