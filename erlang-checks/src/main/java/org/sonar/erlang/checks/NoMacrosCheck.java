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

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.sonar.sslr.api.AstNode;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.NoSqale;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "NoMacros", priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@NoSqale
public class NoMacrosCheck extends SquidCheck<LexerlessGrammar> {

  @RuleProperty(key = "skipDefineInFlowControl", defaultValue = "true",
    description = "Set it false if you want to check macros in flow controls.")
  private boolean skipDefineInFlowControl = true;

  @RuleProperty(key = "allowLiteralMacros", defaultValue = "true",
    description = "Set it to false if you want to have warnings on macros like: -define(TIMEOUT, 1000).")
  private boolean allowLiteralMacros = true;

  @RuleProperty(key = "ignoredMacroNames", defaultValue = "",
    description = "Comma separated list of ignored macro names.")
  private String ignoredMacroNames = "";

  private List<String> ignoreList = new ArrayList<String>();

  Function<String, String> trimItems = new Function<String, String>() {
    @Override
    public String apply(String arg0) {
      return arg0.trim();
    }
  };

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.defineAttr);
    ignoreList.addAll(Lists.transform(Arrays.asList(ignoredMacroNames.split(",")), trimItems));
  }

  @Override
  public void visitNode(AstNode astNode) {
    if (hasFlowControlParent(astNode) && isNotLiteralMacro(astNode) && isNotInIgnoreList(astNode)) {
      getContext().createLineViolation(this, "Do not use macros.", astNode.getTokenLine());
    }
  }

  private boolean isNotInIgnoreList(AstNode astNode) {
    return !ignoreList.contains(getMacroName(astNode));
  }

  private boolean isNotLiteralMacro(AstNode astNode) {
    return (astNode.hasDescendant(ErlangGrammarImpl.funcDecl) && allowLiteralMacros) || (!allowLiteralMacros);
  }

  private boolean hasFlowControlParent(AstNode astNode) {
    return !astNode.hasAncestor(ErlangGrammarImpl.flowControlAttr) || !skipDefineInFlowControl;
  }

  private String getMacroName(AstNode astNode) {
    AstNode token = (astNode.getFirstChild(ErlangGrammarImpl.funcDecl) != null) ? astNode.getFirstChild(ErlangGrammarImpl.funcDecl).getFirstChild(ErlangGrammarImpl.literal)
      : astNode
      .getFirstChild(ErlangGrammarImpl.primaryExpression);
    return token.getTokenOriginalValue();
  }

  public void setSkipDefineInFlowControl(boolean skipDefineInFlowControl) {
    this.skipDefineInFlowControl = skipDefineInFlowControl;
  }

  public void setAllowLiteralMacros(boolean allowLiteralMacros) {
    this.allowLiteralMacros = allowLiteralMacros;
  }

  public void setIgnoredMacroNames(String ignoredMacroNames) {
    this.ignoredMacroNames = ignoredMacroNames;
  }

}
