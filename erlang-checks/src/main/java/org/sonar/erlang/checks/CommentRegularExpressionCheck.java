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

import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.squidbridge.annotations.NoSqale;
import org.sonar.squidbridge.annotations.RuleTemplate;
import org.sonar.squidbridge.checks.AbstractCommentRegularExpressionCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "CommentRegularExpression", priority = Priority.MAJOR)
@NoSqale
@RuleTemplate
public class CommentRegularExpressionCheck extends
  AbstractCommentRegularExpressionCheck<LexerlessGrammar> {

  private static final String DEFAULT_REGULAR_EXPRESSION = "";
  private static final String DEFAULT_MESSAGE = "The violation message";

  @RuleProperty(key = "regularExpression", defaultValue = "" + DEFAULT_REGULAR_EXPRESSION)
  public String regularExpression = DEFAULT_REGULAR_EXPRESSION;

  @RuleProperty(key = "message", defaultValue = "" + DEFAULT_MESSAGE)
  public String message = DEFAULT_MESSAGE;

  @Override
  public String getRegularExpression() {
    return regularExpression;
  }

  @Override
  public String getMessage() {
    return message;
  }

}
