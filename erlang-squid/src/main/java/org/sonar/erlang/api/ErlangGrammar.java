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

import com.sonar.sslr.api.Rule;
import org.sonar.sslr.parser.LexerlessGrammar;

public class ErlangGrammar extends LexerlessGrammar {

  public Rule eof;

  public Rule stringLiteral;
  public Rule regularExpressionLiteral;

  public Rule numericLiteral;
  public Rule identifier;

  public Rule keyword;
  protected Rule letterOrDigit;
//  public Rule comment;
  public Rule spacing;

  public Rule afterKeyword;
  public Rule andKeyword;
  public Rule andalsoKeyword;
  public Rule bandKeyword;
  public Rule beginKeyword;
  public Rule bnotKeyword;
  public Rule borKeyword;
  public Rule bslKeyword;
  public Rule bsrKeyword;
  public Rule bxorKeyword;
  public Rule caseKeyword;
  public Rule catchKeyword;
  public Rule condKeyword;
  public Rule divKeyword;
  public Rule endKeyword;
  public Rule funKeyword;
  public Rule ifKeyword;
  public Rule letKeyword;
  public Rule notKeyword;
  public Rule ofKeyword;
  public Rule orKeyword;
  public Rule orelseKeyword;
  public Rule queryKeyword;
  public Rule receiveKeyword;
  public Rule remKeyword;
  public Rule tryKeyword;
  public Rule whenKeyword;
  public Rule xorKeyword;

  // Puncators
  public Rule arrow;
  public Rule arrowback;
  public Rule doublearrowback;
  public Rule lcurlybrace;
  public Rule rcurlybrace;
  public Rule lparenthesis;
  public Rule rparenthesis;
  public Rule lbracket;
  public Rule rbracket;
  public Rule dot;
  public Rule semi;
  public Rule comma;
  public Rule colon;
  public Rule matchop;
  public Rule plus;
  public Rule minus;
  public Rule star;
  public Rule div;
  public Rule lt;
  public Rule gt;
  public Rule le;
  public Rule ge;
  public Rule equal;
  public Rule notequal;
  public Rule equal2;
  public Rule notequal2;
  public Rule binstart;
  public Rule binend;
  public Rule listcomp;
  public Rule pipe;
  public Rule dollar;
  public Rule apostrophe;
  public Rule plusplus;
  public Rule minusminus;
  public Rule numbersign;
  public Rule exclamation;
  public Rule questionmark;

  public Rule module;
  public Rule functionDeclaration;
  public Rule moduleAttr;
  public Rule exportAttr;
  public Rule compileAttr;
  public Rule defineAttr;
  public Rule typeSpec;
  public Rule genericAttr;
  public Rule anyAttr;
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
  public Rule behaviourAttr;
  public Rule moduleElements;
  public Rule moduleElement;

  @Override
  public Rule getRootRule() {
    return module;
  }

}
