/*
 * SonarQube Erlang Plugin
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

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Trivia;

import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "MethodHasSpecs", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
public class MethodMustHaveSpecs extends SquidCheck<LexerlessGrammar> {

  private static final String DEFAULT_TYPE = "both";

  @RuleProperty(key = "specsType", defaultValue = ""
    + DEFAULT_TYPE)
  private String specsType = DEFAULT_TYPE;

  private SpecTypes type;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.functionDeclaration);
    this.type = SpecTypes.fromString(specsType);
  }

  @Override
  public void leaveNode(AstNode node) {
    boolean result = false;
    switch (type) {
      case BOTH:
        result = checkComment(node) || checkAttribute(node);
        break;
      case ATTRIBUTE:
        result = checkAttribute(node);
        break;
      case COMMENT:
        result = checkComment(node);
        break;
      default:
        break;
    }
    if (!result) {
      getContext()
        .createLineViolation(
          this,
          "Function has no specs in type: " + specsType + ".", node);
    }
  }

  private boolean checkComment(AstNode node) {
    if (node.getToken().hasTrivia()) {
      for (Trivia trivia : node.getToken().getTrivia()) {
        if (trivia.isComment() && trivia.getToken().getOriginalValue().matches("%+ *@spec.*")) {
          return true;
        }
      }
    }
    return false;
  }

  private boolean checkAttribute(AstNode node) {
    return node.getPreviousSibling().getType().equals(ErlangGrammarImpl.spec);
  }

  public void setDefaultSpecsType(String specsType) {
    this.specsType = specsType;
  }

  private enum SpecTypes {
    BOTH, COMMENT, ATTRIBUTE;

    public static SpecTypes fromString(String type) {
      for (SpecTypes t : values()) {
        if (t.toString().equalsIgnoreCase(type)) {
          return t;
        }
      }
      return null;
    }
  }

}
