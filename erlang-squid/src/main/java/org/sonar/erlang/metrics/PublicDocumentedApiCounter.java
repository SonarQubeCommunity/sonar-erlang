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
package org.sonar.erlang.metrics;

import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.api.ErlangPunctuator;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.api.Trivia;
import com.sonar.sslr.squid.SquidAstVisitor;

import java.util.List;

public class PublicDocumentedApiCounter extends SquidAstVisitor<ErlangGrammar> {

  private int numOfPublicAPIs;
  private int numOfPublicDocAPIs;
  private ErlangGrammar g;
  private List<AstNode> functions;

  public PublicDocumentedApiCounter() {
    this.numOfPublicAPIs = 0;
    this.numOfPublicDocAPIs = 0;
  }

  @Override
  public void init() {
    this.g = getContext().getGrammar();
    subscribeTo(g.exportAttr);
  }

  @Override
  public void visitNode(AstNode astNode) {
    /*
     * Ignore all exports in flow control (we cannot decide what to do TODO:
     * analyse common export related flow controls
     */
    if (astNode.findFirstParent(g.flowControlAttr) == null) {
      List<AstNode> exports = astNode.findFirstDirectChild(g.funcExport).findDirectChildren(
          g.funcArity);
      numOfPublicAPIs += exports.size();
      for (AstNode export : exports) {
        AstNode func = findFunctionByArity(getArity(export));
        if (func != null) {
          List<Trivia> comments = func.findFirstChild(GenericTokenType.IDENTIFIER)
              .getToken().getTrivia();
          if (comments.size() > 0) {
            for (Trivia trivia : comments) {
              /**
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
    functions = astNode.findDirectChildren(g.functionDeclaration);
  }

  @Override
  public void leaveFile(AstNode astNode) {
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
    StringBuffer ret = new StringBuffer();
    if ("funcArity".equalsIgnoreCase(node.getName())) {
      for (AstNode arity : node.getChildren()) {
        ret.append(arity.getTokenOriginalValue());
      }
    } else if ("functionDeclaration".equalsIgnoreCase(node.getName())) {
      ret.append(node.findFirstChild(g.funcDecl).getTokenOriginalValue());
      ret.append("/");
      ret.append(node.findFirstChild(g.arguments).findDirectChildren(ErlangPunctuator.COMMA)
          .size() + 1);
    }
    return ret.toString();
  }

}
