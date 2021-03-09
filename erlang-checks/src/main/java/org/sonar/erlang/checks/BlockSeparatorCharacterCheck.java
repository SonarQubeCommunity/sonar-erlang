/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 * Copyright © 2021 Daniils Petrovs <dpetrovs@evolution.com>
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


import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.checks.AbstractCommentRegularExpressionCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "BlockSeparatorCharacter", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleConstantRemediation("1min")
public class BlockSeparatorCharacterCheck extends
  AbstractCommentRegularExpressionCheck<LexerlessGrammar> {

  private static final String REGULAR_EXPRESSION = "^%%+ *([^%s])\\1+ *$";
  private static final String DEFAULT_MESSAGE = "only use '%s' sign(s) for block separators in comments (case sensitive)";

  @RuleProperty(key = "allowedChars", defaultValue = "=")
  public String allowedChars = "=";

  @Override
  public String getMessage() {
    return String.format(DEFAULT_MESSAGE, allowedChars);
  }

  @Override
  public String getRegularExpression() {
    return String.format(REGULAR_EXPRESSION, allowedChars);
  }

}
