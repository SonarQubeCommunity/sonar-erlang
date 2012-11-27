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

import org.sonar.erlang.api.ErlangGrammar;

import com.sonar.sslr.squid.checks.AbstractCommentRegularExpressionCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

@Rule(key = "BlockSeparatorCharacter", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE,
  name = "BlockSeparatorCharacter",
  description = "Only the specified character is allowed as block separator")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class BlockSeparatorCharacterCheck extends
    AbstractCommentRegularExpressionCheck<ErlangGrammar> {

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
