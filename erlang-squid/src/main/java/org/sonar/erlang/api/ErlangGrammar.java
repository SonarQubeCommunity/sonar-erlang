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
package org.sonar.erlang.api;

import com.sonar.sslr.api.Grammar;
import com.sonar.sslr.api.Rule;

public class ErlangGrammar extends Grammar {

  public Rule module;
  public Rule functionDeclaration;
  public Rule moduleAttr;
  public Rule exportAttr;
  public Rule compileAttr;
  public Rule defineAttr;
  public Rule typeSpec;
  public Rule genericAttr;
  public Rule funcExport;
  public Rule expression;
  public Rule funcArity;
  public Rule functionClause;
  public Rule clauseHead;
  public Rule guardSequenceStart;
  public Rule funcDecl;
  public Rule clauseBody;
  public Rule pattern;
  public Rule literal;
  public Rule primaryExpression;
  public Rule listLiteral;
  public Rule tupleLiteral;
  public Rule binaryLiteral;
  public Rule assignmentExpression;
  public Rule memberExpression;
  public Rule funExpression;
  public Rule arguments;
  public Rule unaryExpression;
  public Rule multiplicativeExpression;
  public Rule additiveExpression;
  public Rule shiftExpression;
  public Rule relationalExpression;
  public Rule equalityExpression;
  public Rule bitwiseAndExpression;
  public Rule bitwiseXorExpression;
  public Rule bitwiseOrExpression;
  public Rule logicalAndExpression;
  public Rule logicalOrExpression;
  public Rule leftHandSideExpression;
  public Rule callExpression;
  public Rule qualifier;
  public Rule listOperationExpression;
  public Rule logicalXorExpression;
  public Rule shortCircuitOrElseExpression;
  public Rule shortCircuitAndAlsoExpression;
  public Rule binaryElement;
  public Rule binaryQualifier;
  public Rule expressionStatement;
  public Rule statement;
  public Rule ifExpression;
  public Rule caseExpression;
  public Rule receiveExpression;
  public Rule tryExpression;
  public Rule branchExps;
  public Rule branchExp;
  public Rule guardSequence;
  public Rule eos;
  public Rule guard;
  public Rule guardExpression;
  public Rule functionDeclarationsNoName;
  public Rule functionDeclarationNoName;
  public Rule patternStatements;
  public Rule patternStatement;
  public Rule statements;
  public Rule sendStatement;
  public Rule catchExpression;
  public Rule afterExpression;
  public Rule catchPattern;
  public Rule catchPatternStatement;
  public Rule catchPatternStatements;
  public Rule blockExpression;
  public Rule recordCreateLiteral;
  public Rule recordAccLiteral;
  public Rule recordLiteral;
  public Rule recordLiteralHead;
  public Rule macroLiteral;
  public Rule otherArithmeticExpression;
  public Rule ifdefAttr;
  public Rule ifndefAttr;
  public Rule elseAttr;
  public Rule endifAttr;
  public Rule flowControlAttr;
  public Rule recordAttr;
  public Rule spec;
  public Rule specType;
  public Rule funcSpec;
  public Rule specFun;
  public Rule specTypeDef;
  public Rule specSub;
  public Rule moduleHeadAttr;
  public Rule importAttr;
  public Rule recordField;
  public Rule fileAttr;

  @Override
  public Rule getRootRule() {
    return module;
  }

}
