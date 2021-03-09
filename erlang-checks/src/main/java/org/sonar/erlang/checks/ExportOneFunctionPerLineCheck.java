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

import com.sonar.sslr.api.AstNode;

import java.util.List;
import javax.annotation.Nullable;

import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "ExportOneFunctionPerLine", priority = Priority.MINOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleConstantRemediation("5min")
public class ExportOneFunctionPerLineCheck extends SquidCheck<LexerlessGrammar> {

  private int previousLineNum;
  private String previousFuncArity;

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.exportAttr);
    previousLineNum = 0;
    previousFuncArity = null;
  }

  @Override
  public void visitFile(@Nullable AstNode astNode) {
    previousLineNum = 0;
    previousFuncArity = null;
  }

  @Override
  public void visitNode(AstNode node) {
    /**
     * Get exported func arities in this export
     */
    List<AstNode> funcArities = node.getFirstChild(ErlangGrammarImpl.funcExport)
      .getChildren(ErlangGrammarImpl.funcArity);
    for (AstNode arityNode : funcArities) {
      String funcArity = getArity(arityNode);
      if (previousFuncArity != null) {
        /**
         * If the exported arity is not in the same line but they has
         * the same name
         */
        if (previousLineNum != arityNode.getTokenLine()
          && getFuncName(previousFuncArity).equals(getFuncName(funcArity))) {
          getContext()
            .createLineViolation(
              this,
              "The exported method with arity: {0} is in different line, but it has the same name as the previous arity: {1}.",
              arityNode.getTokenLine(), funcArity, previousFuncArity);
        }
        /**
         * If exported arity is in the same line but has different name
         */
        if (previousLineNum == arityNode.getTokenLine()
          && !getFuncName(previousFuncArity).equals(getFuncName(funcArity))) {
          getContext()
            .createLineViolation(
              this,
              "The exported method with arity: {0} is in the same line, but it has different name than the previous arity: {1}.",
              arityNode.getTokenLine(), funcArity, previousFuncArity);
        }
      }
      previousFuncArity = funcArity;
      previousLineNum = arityNode.getTokenLine();
    }
  }

  private String getFuncName(String arity) {
    return arity.substring(0, arity.lastIndexOf('/'));
  }

  private String getArity(AstNode arityNode) {
    StringBuilder ret = new StringBuilder();
    for (AstNode arity : arityNode.getChildren()) {
      ret.append(arity.getTokenOriginalValue());
    }
    return ret.toString();
  }

}
