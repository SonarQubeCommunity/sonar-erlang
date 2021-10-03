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
package org.sonar.erlang.metrics;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Trivia;

import org.sonar.squidbridge.SquidAstVisitor;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.util.List;

public class PublicDocumentedApiCounter extends SquidAstVisitor<LexerlessGrammar> {

  private double numOfPublicAPIs;
  private double numOfPublicDocAPIs;
  private List<AstNode> functions;

  public PublicDocumentedApiCounter() {
    this.numOfPublicAPIs = 0;
    this.numOfPublicDocAPIs = 0;
  }

  @Override
  public void init() {
    subscribeTo(ErlangGrammarImpl.exportAttr);
  }

  @Override
  public void visitNode(AstNode astNode) {
    /*
     * Ignore all exports in flow control (we cannot decide what to do TODO:
     * analyse common export related flow controls
     */
    if (astNode.getFirstAncestor(ErlangGrammarImpl.flowControlAttr) == null) {
      List<AstNode> exports = astNode.getFirstChild(ErlangGrammarImpl.funcExport).getChildren(
        ErlangGrammarImpl.funcArity);
      numOfPublicAPIs += exports.size();
      for (AstNode export : exports) {
        AstNode func = findFunctionByArity(getArity(export));
        if (func != null) {
          List<Trivia> comments = func.getFirstDescendant(ErlangGrammarImpl.atom)
            .getToken().getTrivia();
          if (!comments.isEmpty()) {
            for (Trivia trivia : comments) {
              /*
               * Try to filter out those comments what has only
               * one repeated char
               */
              if (trivia.isComment()
                && !trivia.getToken().getOriginalValue().matches(
                "^%%+ *(.)\\1+ *$")) {
                numOfPublicDocAPIs++;
                break;
              }
            }
          }
        }
      }

    }
  }

  @Override
  public void visitFile(AstNode astNode) {
    if (astNode == null) {
      // file wasn't parsed
      return;
    }
    functions = astNode.getFirstChild(ErlangGrammarImpl.moduleElements).getChildren(ErlangGrammarImpl.functionDeclaration);
  }

  @Override
  public void leaveFile(AstNode astNode) {
    if (astNode == null) {
      // file wasn't parsed
      return;
    }
    getContext().peekSourceCode().add(ErlangMetric.PUBLIC_API, numOfPublicAPIs);
    getContext().peekSourceCode().add(ErlangMetric.PUBLIC_DOC_API, numOfPublicDocAPIs);
    double density = (numOfPublicAPIs > 0) ? numOfPublicDocAPIs / numOfPublicAPIs : 0;
    getContext().peekSourceCode().add(ErlangMetric.PUBLIC_DOCUMENTED_API_DENSITY, density);
    functions = null;
  }

  private AstNode findFunctionByArity(String arity) {
    for (AstNode function : functions) {
      if (getArity(function).equals(arity)) {
        return function;
      }
    }
    return null;
  }

  private String getArity(AstNode node) {
    StringBuilder ret = new StringBuilder();
    if ("funcArity".equalsIgnoreCase(node.getName())) {
      for (AstNode arity : node.getChildren()) {
        ret.append(arity.getTokenOriginalValue());
      }
    } else if ("functionDeclaration".equalsIgnoreCase(node.getName())) {
      ret.append(node.getFirstDescendant(ErlangGrammarImpl.funcDecl).getTokenOriginalValue());
      ret.append("/");
      ret.append(node.getFirstDescendant(ErlangGrammarImpl.arguments).getChildren(ErlangGrammarImpl.comma)
        .size() + 1);
    }
    return ret.toString();
  }

}
