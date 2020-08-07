/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.sonar.erlang.checks;


import org.sonar.squidbridge.checks.CheckMessagesVerifier;
import org.junit.Test;

import org.sonar.squidbridge.api.SourceFile;

import java.io.File;

public class SpaceAfterBeforeOperatorsCheckTest {

  @Test
  public void test() {
    SpaceAfterBeforeOperatorsCheck check = new SpaceAfterBeforeOperatorsCheck();

    SourceFile file = TestHelper.scanSingleFile(new File(
      "src/test/resources/checks/spaceafteroperator.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
      .next().atLine(4).withMessage("No space after operator in column: 9.")
      .next().atLine(5).withMessage("No space after operator in column: 12.")
      .next().atLine(6).withMessage("No space after operator in column: 5.")
      .next().atLine(7).withMessage("No space after operator in column: 8.")
      .next().atLine(9).withMessage("No space after operator in column: 13.")
      .next().atLine(12).withMessage("No space after operator in column: 5.")
      .next().atLine(14).withMessage("No space after operator in column: 8.").noMore();
  }

}
