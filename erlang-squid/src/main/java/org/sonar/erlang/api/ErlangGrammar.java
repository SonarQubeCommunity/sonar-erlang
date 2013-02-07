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

  protected Rule eof;

  public Rule stringLiteral;
  public Rule regularExpressionLiteral;

  protected Rule numericLiteral;
  protected Rule identifier;

  protected Rule keyword;
  protected Rule letterOrDigit;
  protected Rule spacing;

  protected Rule afterKeyword;
  protected Rule andKeyword;
  protected Rule andalsoKeyword;
  protected Rule bandKeyword;
  protected Rule beginKeyword;
  protected Rule bnotKeyword;
  protected Rule borKeyword;
  protected Rule bslKeyword;
  protected Rule bsrKeyword;
  protected Rule bxorKeyword;
  protected Rule caseKeyword;
  protected Rule catchKeyword;
  protected Rule condKeyword;
  protected Rule divKeyword;
  protected Rule endKeyword;
  protected Rule funKeyword;
  protected Rule ifKeyword;
  protected Rule letKeyword;
  protected Rule notKeyword;
  protected Rule ofKeyword;
  protected Rule orKeyword;
  protected Rule orelseKeyword;
  protected Rule queryKeyword;
  protected Rule receiveKeyword;
  protected Rule remKeyword;
  protected Rule tryKeyword;
  protected Rule whenKeyword;
  protected Rule xorKeyword;

  // Puncators
  protected Rule arrow;
  protected Rule arrowback;
  protected Rule doublearrowback;
  protected Rule lcurlybrace;
  protected Rule rcurlybrace;
  protected Rule lparenthesis;
  protected Rule rparenthesis;
  protected Rule lbracket;
  protected Rule rbracket;
  protected Rule dot;
  protected Rule semi;
  protected Rule comma;
  protected Rule colon;
  protected Rule matchop;
  protected Rule plus;
  protected Rule minus;
  protected Rule star;
  protected Rule div;
  protected Rule lt;
  protected Rule gt;
  protected Rule le;
  protected Rule ge;
  protected Rule equal;
  protected Rule notequal;
  protected Rule equal2;
  protected Rule notequal2;
  protected Rule binstart;
  protected Rule binend;
  protected Rule listcomp;
  protected Rule pipe;
  protected Rule dollar;
  protected Rule apostrophe;
  protected Rule plusplus;
  protected Rule minusminus;
  protected Rule numbersign;
  protected Rule exclamation;
  protected Rule questionmark;

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
