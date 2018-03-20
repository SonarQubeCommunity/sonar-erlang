/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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

public class MethodMustHaveSpecsTest {

  @Test
  public void test() {
    MethodMustHaveSpecs check = new MethodMustHaveSpecs();

    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/methodmusthavespecs.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(15).withMessage("Function has no specs in type: both.")
      .noMore();
  }

  @Test
  public void test2() {
    MethodMustHaveSpecs check = new MethodMustHaveSpecs();
    check.setDefaultSpecsType("attribute");
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/methodmusthavespecs.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(15).withMessage("Function has no specs in type: attribute.")
      .next().atLine(19).withMessage("Function has no specs in type: attribute.")
      .noMore();
  }

  @Test
  public void test3() {
    MethodMustHaveSpecs check = new MethodMustHaveSpecs();
    check.setDefaultSpecsType("comment");
    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/methodmusthavespecs.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(6).withMessage("Function has no specs in type: comment.")
      .next().atLine(11).withMessage("Function has no specs in type: comment.")
      .next().atLine(15).withMessage("Function has no specs in type: comment.")
      .noMore();
  }


}
