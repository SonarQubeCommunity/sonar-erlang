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
package org.sonar.erlang.parser;

import com.sonar.sslr.api.GenericTokenType;
import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangKeyword;
import org.sonar.erlang.api.ErlangPunctuator;
import org.sonar.erlang.api.ErlangTokenType;
import org.sonar.erlang.lexer.ErlangLexer;

import static org.sonar.sslr.parser.GrammarOperators.endOfInput;
import static org.sonar.sslr.parser.GrammarOperators.firstOf;
import static org.sonar.sslr.parser.GrammarOperators.nextNot;
import static org.sonar.sslr.parser.GrammarOperators.oneOrMore;
import static org.sonar.sslr.parser.GrammarOperators.optional;
import static org.sonar.sslr.parser.GrammarOperators.regexp;
import static org.sonar.sslr.parser.GrammarOperators.sequence;
import static org.sonar.sslr.parser.GrammarOperators.token;
import static org.sonar.sslr.parser.GrammarOperators.zeroOrMore;

public class ErlangGrammarImpl extends ErlangGrammar {

  public ErlangGrammarImpl() {
    lexical();
    punctuators();
    keywords();
    expressions();
    branchAndGuardExpressions();
    statements();
    module();
    functions();
    // GrammarFunctions.enableMemoizationOfMatchesForAllRules(this);
  }

  /**
   * Lexical
   */

  private void lexical() {
    eof.is(token(GenericTokenType.EOF, endOfInput())).skip();
    identifier.is(
        nextNot(keyword),
        token(GenericTokenType.IDENTIFIER, regexp(ErlangLexer.IDENTIFIER)), spacing).skip();
    numericLiteral.is(
        token(ErlangTokenType.NUMERIC_LITERAL,
            regexp(ErlangLexer.NUMERIC_LITERAL)), spacing);
    stringLiteral.is(
        token(GenericTokenType.LITERAL,
            regexp(ErlangLexer.LITERAL)), spacing);

    keyword.is(firstOf(
        "after",
        "and",
        "andalso",
        "band",
        "begin",
        "bnot",
        "bor",
        "bsl",
        "bsr",
        "bxor",
        "case",
        "catch",
        "cond",
        "div",
        "end",
        "fun",
        "if",
        "let",
        "not",
        "of",
        "or",
        "orelse",
        "query",
        "receive",
        "rem",
        "try",
        "when",
        "xor"), nextNot(letterOrDigit));
    letterOrDigit.is(regexp("\\p{javaJavaIdentifierPart}"));

    spacing.is(regexp(ErlangLexer.WHITESPACE + "*+"),
        zeroOrMore(
            token(GenericTokenType.COMMENT, regexp(ErlangLexer.COMMENT)),
            regexp(ErlangLexer.WHITESPACE + "*+"))).skip();
  }

  private void punctuators() {
    arrow.is(punctuator("->")).skip();
    arrowback.is(punctuator("<-")).skip();
    doublearrowback.is(punctuator("<=")).skip();
    lcurlybrace.is(punctuator("{")).skip();
    rcurlybrace.is(punctuator("}")).skip();
    lparenthesis.is(punctuator("(")).skip();
    rparenthesis.is(punctuator(")")).skip();
    lbracket.is(punctuator("[")).skip();
    rbracket.is(punctuator("]")).skip();
    dot.is(punctuator(".")).skip();
    semi.is(punctuator(";")).skip();
    comma.is(punctuator(",")).skip();
    colon.is(punctuator(":")).skip();
    matchop.is(punctuator("=", nextNot(firstOf("=", "<", ":", "/")))).skip();
    plus.is(punctuator("+", nextNot("+"))).skip();
    minus.is(punctuator("-", nextNot(firstOf(">", "-")))).skip();
    star.is(punctuator("*")).skip();
    div.is(punctuator("/", nextNot("="))).skip();
    lt.is(punctuator("<", nextNot(firstOf("=", "<")))).skip();
    gt.is(punctuator(">", nextNot(firstOf("=", ">")))).skip();
    le.is(punctuator("=<")).skip();
    ge.is(punctuator(">=")).skip();
    equal.is(punctuator("==")).skip();
    notequal.is(punctuator("/=")).skip();
    equal2.is(punctuator("=:=")).skip();
    notequal2.is(punctuator("=/=")).skip();
    binstart.is(punctuator("<<")).skip();
    binend.is(punctuator(">>")).skip();
    listcomp.is(punctuator("||")).skip();
    pipe.is(punctuator("|", nextNot("|"))).skip();
    dollar.is(punctuator("$")).skip();
    apostrophe.is(punctuator("'")).skip();
    plusplus.is(punctuator("++")).skip();
    minusminus.is(punctuator("--")).skip();
    numbersign.is(punctuator("#")).skip();
    exclamation.is(punctuator("!")).skip();
    questionmark.is(punctuator("?")).skip();
  }

  private void keywords() {
    afterKeyword.is(keyword("after")).skip();
    andKeyword.is(keyword("and")).skip();
    andalsoKeyword.is(keyword("andalso")).skip();
    bandKeyword.is(keyword("band")).skip();
    beginKeyword.is(keyword("begin")).skip();
    bnotKeyword.is(keyword("bnot")).skip();
    borKeyword.is(keyword("bor")).skip();
    bslKeyword.is(keyword("bsl")).skip();
    bsrKeyword.is(keyword("bsr")).skip();
    bxorKeyword.is(keyword("bxor")).skip();
    caseKeyword.is(keyword("case")).skip();
    catchKeyword.is(keyword("catch")).skip();
    condKeyword.is(keyword("cond")).skip();
    divKeyword.is(keyword("div")).skip();
    endKeyword.is(keyword("end")).skip();
    funKeyword.is(keyword("fun")).skip();
    ifKeyword.is(keyword("if")).skip();
    letKeyword.is(keyword("let")).skip();
    notKeyword.is(keyword("not")).skip();
    ofKeyword.is(keyword("of")).skip();
    orKeyword.is(keyword("or")).skip();
    orelseKeyword.is(keyword("orelse")).skip();
    queryKeyword.is(keyword("query")).skip();
    receiveKeyword.is(keyword("receive")).skip();
    remKeyword.is(keyword("rem")).skip();
    tryKeyword.is(keyword("try")).skip();
    whenKeyword.is(keyword("when")).skip();
    xorKeyword.is(keyword("xor")).skip();
  }

  private void module() {
    module.is(spacing, optional(moduleElements), eof);
    moduleElements.is(oneOrMore(
        // TODO: does the -module mandatory? maybe we should move it one level
        // up
        moduleElement
        ));

    moduleElement.is(firstOf(moduleHeadAttr, sequence(macroLiteral, dot), functionDeclaration)).skipIfOneChild();

    moduleHeadAttr.is(firstOf(moduleAttr, fileAttr, exportAttr, compileAttr, defineAttr,
        importAttr, typeSpec, spec, recordAttr, flowControlAttr, behaviourAttr, genericAttr)).skipIfOneChild();

    recordAttr.is(minus, semiKeyword("record"), lparenthesis, identifier, comma, lcurlybrace, optional(sequence(
        recordField, optional(matchop, recordField)), zeroOrMore(firstOf(comma, pipe), sequence(recordField,
        optional(matchop, recordField)))), rcurlybrace, rparenthesis, dot);

    recordField.is(firstOf(sequence(firstOf(lcurlybrace, lbracket), recordField, zeroOrMore(comma,
        recordField), firstOf(rcurlybrace, rbracket)),
        sequence(firstOf(specFun, callExpression))), optional(colon, colon, recordField));

    flowControlAttr.is(firstOf(ifdefAttr, ifndefAttr), oneOrMore(firstOf(moduleHeadAttr,
        functionDeclaration)), optional(elseAttr, oneOrMore(firstOf(moduleHeadAttr,
        functionDeclaration))), endifAttr);

    ifdefAttr.is(minus, semiKeyword("ifdef"), lparenthesis, identifier, rparenthesis, dot);

    ifndefAttr.is(minus, semiKeyword("ifndef"), lparenthesis, identifier, rparenthesis, dot);

    elseAttr.is(minus, semiKeyword("else"), dot);

    endifAttr.is(minus, semiKeyword("endif"), dot);

    moduleAttr.is(minus, semiKeyword("module"), lparenthesis, identifier, rparenthesis, dot);
    exportAttr.is(minus, semiKeyword("export"), lparenthesis, funcExport, rparenthesis, dot);
    compileAttr.is(minus, semiKeyword("compile"), lparenthesis, primaryExpression, rparenthesis, dot);

    defineAttr.is(minus, semiKeyword("define"), lparenthesis, firstOf(sequence(identifier, comma,
        statement), sequence(funcDecl, comma, statement)), rparenthesis, dot);

    importAttr.is(minus, semiKeyword("import"), lparenthesis, firstOf(macroLiteral, identifier), comma,
        lbracket, funcArity, zeroOrMore(comma, funcArity), rbracket, rparenthesis, dot);

    fileAttr.is(minus, semiKeyword("file"), lparenthesis, primaryExpression, comma, primaryExpression,
        rparenthesis, dot);

    behaviourAttr.is(minus, semiKeyword("behaviour"), lparenthesis, identifier, rparenthesis, dot);

    genericAttr.is(minus, firstOf(semiKeyword("vsn"), semiKeyword("on_load"), semiKeyword("include"), semiKeyword("file"),
        semiKeyword("ignore_xref"), semiKeyword("include_lib"), semiKeyword("author"), semiKeyword("export_type"), semiKeyword("deprecated"), semiKeyword("asn1_info")),
        lparenthesis, firstOf(funcArity, primaryExpression), rparenthesis, dot);
    // TODO: is it possible to have something like: -export().?
    funcExport.is(lbracket, zeroOrMore(funcArity, zeroOrMore(comma, funcArity)), rbracket);
  }

  private void functions() {
    spec.is(minus, firstOf(semiKeyword("spec"), semiKeyword("callback")), optional(lparenthesis), optional(
        identifier, colon), identifier, optional(div, numericLiteral), optional(colon, colon),
        funcSpec, zeroOrMore(semi, funcSpec), optional(rparenthesis), dot);

    typeSpec.is(minus, firstOf(semiKeyword("type"), semiKeyword("opaque")), optional(lparenthesis), funcDecl, firstOf(sequence(
        colon, colon, specType), sequence(arrow, funcDecl)), optional(rparenthesis), dot);

    funcSpec.is(lparenthesis, optional(specType), rparenthesis, arrow, specType, optional(whenKeyword, specType));

    specType.is(oneOrMore(specTypeDef, zeroOrMore(firstOf(pipe, comma), specTypeDef)));

    specTypeDef.is(firstOf(sequence(firstOf(lcurlybrace, lbracket), specTypeDef, firstOf(
        rcurlybrace, rbracket)), specSub), zeroOrMore(firstOf(comma, pipe), firstOf(sequence(firstOf(
        lcurlybrace, lbracket), specTypeDef, firstOf(rcurlybrace, rbracket)), specSub)));

    specSub.is(firstOf(
        // things in ()
        sequence(lparenthesis, specTypeDef, rparenthesis),
        // workaround for fun like expression:fun(), fun((id())-> error
        // | ok)
        sequence(specFun),
        // something like: list(A | B)
        sequence(identifier, lparenthesis, callExpression, oneOrMore(pipe, callExpression),
            rparenthesis),
        // and: Mega::giga()
        sequence(firstOf(funcArity, identifier), colon, colon, specTypeDef),
        // and for records
        sequence(numbersign, identifier, specTypeDef),
        // and things like: 1..255
        sequence(primaryExpression, dot, dot, primaryExpression),
        // or just simple ...
        sequence(dot, dot, dot),
        // and simple function call
        sequence(optional(identifier, colon), identifier, lparenthesis, optional(specTypeDef),
            rparenthesis),
        // and everything other
        callExpression));

    specFun.is(funKeyword, lparenthesis, optional(lparenthesis, optional(specTypeDef), rparenthesis), optional(arrow,
        specTypeDef), rparenthesis);

    functionDeclaration.is(functionClause, zeroOrMore(semi, functionClause),

        dot);
    functionClause.is(clauseHead, arrow, clauseBody);
    clauseHead.is(funcDecl, optional(guardSequenceStart));
    clauseBody.is(statements);

    funcArity.is(optional(literal, colon), literal, div, literal);

    funcDecl.is(identifier, arguments);
  }

  private void expressions() {
    // handle string concetanation ("..."\n[\r\t]"..." is one literal as
    // well this:
    // "asasd" ?MACRO "asdasd"
    literal.is(oneOrMore(firstOf(stringLiteral, numericLiteral, identifier, macroLiteral)/*
                                                                                          * ,
                                                                                          * zeroOrMore(firstOf(literal, macroLiteral))
                                                                                          */));
    primaryExpression.is(firstOf(sequence(lparenthesis, expression, rparenthesis), literal, listLiteral, tupleLiteral, binaryLiteral));

    listLiteral.is(lbracket, optional(firstOf(sequence(assignmentExpression, listcomp, qualifier, zeroOrMore(
        comma, qualifier)), sequence(assignmentExpression, zeroOrMore(firstOf(comma,
        assignmentExpression)), optional(pipe, assignmentExpression)))), rbracket);
    qualifier.is(firstOf(sequence(assignmentExpression, arrowback, expression), expression));
    recordLiteral.is(optional(primaryExpression), oneOrMore(recordLiteralHead), optional(lcurlybrace, optional(
        assignmentExpression, zeroOrMore(comma, assignmentExpression)), rcurlybrace));
    recordLiteralHead.is(numbersign, identifier, zeroOrMore(dot, identifier));

    macroLiteral.is(questionmark, identifier, optional(arguments));
    tupleLiteral.is(lcurlybrace, zeroOrMore(firstOf(comma, expression)), rcurlybrace);
    binaryLiteral.is(binstart, firstOf(sequence(sequence(assignmentExpression, listcomp,
        oneOrMore(binaryQualifier)), zeroOrMore(firstOf(comma, assignmentExpression))), zeroOrMore(firstOf(
        comma, binaryElement))), binend);
    binaryQualifier.is(firstOf(
        sequence(binaryLiteral, doublearrowback, expression), sequence(
            primaryExpression, arrowback, expression, zeroOrMore(comma, expression)

        )));

    binaryElement.is(firstOf(sequence(expression, optional(colon, firstOf(numericLiteral, identifier,
        macroLiteral)), optional(div,
        /*
         * Hack for things like: 1024:32/little-float-dafaq
         */
        firstOf(numericLiteral, sequence(identifier, oneOrMore(minus, identifier)), identifier)))));
    memberExpression.is(
        firstOf(recordLiteral, macroLiteral, ifExpression, funExpression, caseExpression,
            tryExpression, receiveExpression, blockExpression, primaryExpression))
        .skipIfOneChild();
    /**
     * It can be a record ref (originaly a.b['a']) as well
     */
    callExpression.is(
        firstOf(sequence(optional(memberExpression, colon), memberExpression, arguments),
            memberExpression)).skipIfOneChild();

    arguments.is(lparenthesis, optional(assignmentExpression, zeroOrMore(comma, assignmentExpression)),
        rparenthesis);
    unaryExpression.is(firstOf(
        // handle things like: -12, -A, -func(A), -(6+3)
        sequence(optional(minus), callExpression), sequence(notKeyword, unaryExpression))).skipIfOneChild();
    otherArithmeticExpression.is(unaryExpression,
        zeroOrMore(firstOf(bnotKeyword, divKeyword, remKeyword), unaryExpression)).skipIfOneChild();
    multiplicativeExpression.is(otherArithmeticExpression,
        zeroOrMore(firstOf(star, div), otherArithmeticExpression)).skipIfOneChild();
    additiveExpression.is(multiplicativeExpression,
        zeroOrMore(firstOf(plus, minus), multiplicativeExpression)).skipIfOneChild();

    shiftExpression.is(additiveExpression, zeroOrMore(firstOf(bslKeyword, bsrKeyword), additiveExpression))
        .skipIfOneChild();
    relationalExpression.is(shiftExpression, zeroOrMore(firstOf(lt, gt, le, ge), shiftExpression))
        .skipIfOneChild();

    equalityExpression.is(relationalExpression,
        zeroOrMore(firstOf(equal, notequal, equal2, notequal2), relationalExpression))
        .skipIfOneChild();

    bitwiseAndExpression.is(equalityExpression, zeroOrMore(bandKeyword, equalityExpression)).skipIfOneChild();

    bitwiseXorExpression.is(bitwiseAndExpression, zeroOrMore(bxorKeyword, bitwiseAndExpression))
        .skipIfOneChild();

    bitwiseOrExpression.is(bitwiseXorExpression, zeroOrMore(borKeyword, bitwiseXorExpression))
        .skipIfOneChild();

    logicalAndExpression.is(bitwiseOrExpression, zeroOrMore(andKeyword, bitwiseOrExpression))
        .skipIfOneChild();

    logicalOrExpression.is(logicalAndExpression, zeroOrMore(orKeyword, logicalAndExpression))
        .skipIfOneChild();

    logicalXorExpression.is(logicalOrExpression, zeroOrMore(xorKeyword, logicalOrExpression))
        .skipIfOneChild();

    shortCircuitOrElseExpression.is(logicalXorExpression, zeroOrMore(orelseKeyword, logicalXorExpression))
        .skipIfOneChild();

    shortCircuitAndAlsoExpression.is(shortCircuitOrElseExpression,
        zeroOrMore(andalsoKeyword, shortCircuitOrElseExpression)).skipIfOneChild();

    listOperationExpression.is(shortCircuitAndAlsoExpression,
        zeroOrMore(firstOf(plusplus, minusminus), shortCircuitAndAlsoExpression)).skipIfOneChild();

    assignmentExpression.is(
        firstOf(sequence(listOperationExpression, matchop, assignmentExpression),
            listOperationExpression)).skipIfOneChild();

    expression.is(optional(catchKeyword), assignmentExpression);

    funExpression.is(funKeyword, firstOf(sequence(optional(memberExpression, colon), funcArity),
        sequence(functionDeclarationsNoName, endKeyword)), optional(arguments));
    functionDeclarationsNoName.is(functionDeclarationNoName, zeroOrMore(semi,
        functionDeclarationNoName));
    functionDeclarationNoName.is(arguments, optional(guardSequenceStart), arrow, statements);

    caseExpression.is(caseKeyword, expression, ofKeyword, patternStatements, endKeyword);

    ifExpression.is(ifKeyword, branchExps, endKeyword);

    tryExpression.is(tryKeyword, statements, optional(ofKeyword, patternStatements), firstOf(sequence(catchExpression,
        afterExpression), catchExpression, afterExpression), endKeyword);

    afterExpression.is(afterKeyword, statements);

    catchExpression.is(catchKeyword, catchPatternStatements);

    receiveExpression.is(receiveKeyword, firstOf(sequence(patternStatements, optional(afterKeyword, expression, arrow,
        statements)), sequence(afterKeyword, expression, arrow, statements)), endKeyword);

    blockExpression.is(beginKeyword, statements, endKeyword);
  }

  /**
   * A.4 Statement
   **/
  private void statements() {
    expressionStatement.is(expression);
    statement.is(firstOf(sendStatement, expressionStatement));
    statements.is(statement, zeroOrMore(comma, statement));

    sendStatement.is(expression, exclamation, expression);
  }

  public void branchAndGuardExpressions() {
    branchExps.is(branchExp, zeroOrMore(semi, branchExp));
    branchExp.is(guardSequence, arrow, statements);

    patternStatements.is(patternStatement, zeroOrMore(semi, patternStatement));
    patternStatement.is(pattern, optional(guardSequenceStart), arrow, statements);

    catchPatternStatements.is(catchPatternStatement, zeroOrMore(semi, catchPatternStatement));
    catchPatternStatement.is(catchPattern, optional(guardSequenceStart), arrow, statements);
    pattern.is(assignmentExpression);
    catchPattern.is(optional(identifier, colon), assignmentExpression);

    guardSequenceStart.is(whenKeyword, guardSequence);

    guardSequence.is(guard, zeroOrMore(semi, guard));
    guard.is(guardExpression, zeroOrMore(comma, guardExpression));
    guardExpression.is(expression);
  }

  private Object punctuator(String value) {
    for (ErlangPunctuator tokenType : ErlangPunctuator.values()) {
      if (value.equals(tokenType.getValue())) {
        return sequence(token(tokenType, value), spacing);
      }
    }
    throw new IllegalStateException(value);
  }

  private Object punctuator(String value, Object element) {
    for (ErlangPunctuator tokenType : ErlangPunctuator.values()) {
      if (value.equals(tokenType.getValue())) {
        return sequence(token(tokenType, value), element, spacing);
      }
    }
    throw new IllegalStateException(value);
  }

  private Object keyword(String value) {
    for (ErlangKeyword tokenType : ErlangKeyword.values()) {
      if (value.equals(tokenType.getValue())) {
        return sequence(token(tokenType, value), nextNot(letterOrDigit), spacing);
      }
    }
    throw new IllegalStateException(value);
  }

  private Object semiKeyword(String value) {
    return sequence(token(GenericTokenType.IDENTIFIER, value), nextNot(letterOrDigit), spacing);
  }
}
