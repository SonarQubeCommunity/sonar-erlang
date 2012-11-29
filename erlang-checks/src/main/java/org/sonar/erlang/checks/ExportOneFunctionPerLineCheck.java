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

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import java.util.List;

@Rule(key = "ExportOneFunctionPerLine", priority = Priority.MINOR,
  cardinality = Cardinality.SINGLE, name = "ExportOneFunctionPerLine",
  description =
      "<p>For readablity reasons logically group your -export(..) declarations.<br/>" +
      "At most one -export(..) block per group, where group can be (in this order):<br/>"+
      "<ul><li>the public API of a module</li>"+
      "<li>callbacks for behaviour (gen_server, etc)</li>"+
      "<li>extra exports for tests (ifdef(TEST)&lt;..&gt;)</li>"+
      "</ul><br/>" +
      "Export one function per line at least for the public API (except for same name function" +
      "of different arities)<br/>" +
      "%% API<br/>"+
      "-export([<br/>"+
      "add/2,<br/>"+
      "divide/2,<br/>"+
      "increment/1, increment/2,<br/>"+
      "mult/2,<br/>"+
      "substract/2<br/>"+
      "]).</p>")
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
public class ExportOneFunctionPerLineCheck extends SquidCheck<ErlangGrammar> {

  private int previousLineNum;
  private String previousFuncArity;
  private ErlangGrammar grammar;

  @Override
  public void init() {
    grammar = getContext().getGrammar();
    subscribeTo(grammar.exportAttr);
    previousLineNum = 0;
    previousFuncArity = null;
  }

  @Override
  public void visitNode(AstNode node) {
    /**
     * Get exported func arities in this export
     */
    List<AstNode> funcArities = node.findFirstDirectChild(grammar.funcExport)
        .findDirectChildren(grammar.funcArity);
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
    return arity.substring(0, arity.lastIndexOf("/"));
  }

  private String getArity(AstNode arityNode) {
    StringBuffer ret = new StringBuffer();
    for (AstNode arity : arityNode.getChildren()) {
      ret.append(arity.getTokenOriginalValue());
    }
    return ret.toString();
  }

}
