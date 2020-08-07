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

import com.google.common.collect.ImmutableList;

import java.util.List;

public final class CheckList {

  public static final String REPOSITORY_KEY = "erlang";

  public static final String REPOSITORY_NAME = "Sonar";

  private CheckList() {
  }

  public static List<Class> getChecks() {
    return ImmutableList.of(BlockSeparatorCharacterCheck.class,
      BranchesOfRecursionCheck.class, CommentRegularExpressionCheck.class,
      DepthOfCasesCheck.class, DoNotUseEmptyFlowControlCheck.class, DoNotUseImportCheck.class, DoNotUseExportAllCheck.class,
      ExportOneFunctionPerLineCheck.class, FunctionComplexityCheck.class, FunExpressionComplexityCheck.class,
      FunctionDefAndClausesSeparationCheck.class, IndentionSizeCheck.class,
      LineLengthCheck.class, MultipleBlankLinesCheck.class,
      NoEmacsStyleLeadingCommasCheck.class, NoSpaceAfterBeforeBracketsCheck.class,
      NoTabsForIndentionCheck.class, NoTrailingWhitespaceCheck.class,
      NumberOfFunctionArgsCheck.class, ParsingErrorCheck.class, SpaceAfterBeforeOperatorsCheck.class, XPathCheck.class,
      IsTailRecursiveCheck.class, MethodMustHaveSpecs.class, TodoCommentCheck.class, FixmeCommentCheck.class,
      FunctionLengthCheck.class);
  }

}
