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
package org.sonar.erlang.checks;


import org.sonar.squidbridge.checks.CheckMessagesVerifier;
import org.junit.Test;

import org.sonar.squidbridge.api.SourceFile;

import java.io.File;

public class BranchesOfRecursionCheckTest {

  @Test
  public void test() {
    BranchesOfRecursionCheck check = new BranchesOfRecursionCheck();

    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/branchesofrecursion.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages()).noMore();
  }

  @Test
  public void test2() {
    BranchesOfRecursionCheck check = new BranchesOfRecursionCheck();
    check.setMaximumBORThreshold(1);
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/branchesofrecursion.erl"), check);

    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(2).withMessage("Function has 2 branches of recursion which is greater than 1 authorized.")
      .next().atLine(19).withMessage("Function has 3 branches of recursion which is greater than 1 authorized.")
      .noMore();
  }
}
