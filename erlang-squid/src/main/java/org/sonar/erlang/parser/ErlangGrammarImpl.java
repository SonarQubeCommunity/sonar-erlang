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

import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangKeyword;
import org.sonar.erlang.api.ErlangPunctuator;

import static org.sonar.erlang.api.ErlangKeyword.AFTER;
import static org.sonar.erlang.api.ErlangKeyword.AND;
import static org.sonar.erlang.api.ErlangKeyword.ANDALSO;
import static org.sonar.erlang.api.ErlangKeyword.BAND;
import static org.sonar.erlang.api.ErlangKeyword.BEGIN;
import static org.sonar.erlang.api.ErlangKeyword.BNOT;
import static org.sonar.erlang.api.ErlangKeyword.BOR;
import static org.sonar.erlang.api.ErlangKeyword.BSL;
import static org.sonar.erlang.api.ErlangKeyword.BSR;
import static org.sonar.erlang.api.ErlangKeyword.BXOR;
import static org.sonar.erlang.api.ErlangKeyword.CASE;
import static org.sonar.erlang.api.ErlangKeyword.CATCH;
import static org.sonar.erlang.api.ErlangKeyword.END;
import static org.sonar.erlang.api.ErlangKeyword.FUN;
import static org.sonar.erlang.api.ErlangKeyword.IF;
import static org.sonar.erlang.api.ErlangKeyword.NOT;
import static org.sonar.erlang.api.ErlangKeyword.OF;
import static org.sonar.erlang.api.ErlangKeyword.OR;
import static org.sonar.erlang.api.ErlangKeyword.ORELSE;
import static org.sonar.erlang.api.ErlangKeyword.RECEIVE;
import static org.sonar.erlang.api.ErlangKeyword.REM;
import static org.sonar.erlang.api.ErlangKeyword.TRY;
import static org.sonar.erlang.api.ErlangKeyword.WHEN;
import static org.sonar.erlang.api.ErlangKeyword.XOR;
import static org.sonar.erlang.api.ErlangPunctuator.ARROW;
import static org.sonar.erlang.api.ErlangPunctuator.ARROWBACK;
import static org.sonar.erlang.api.ErlangPunctuator.BINEND;
import static org.sonar.erlang.api.ErlangPunctuator.BINSTART;
import static org.sonar.erlang.api.ErlangPunctuator.COLON;
import static org.sonar.erlang.api.ErlangPunctuator.COMMA;
import static org.sonar.erlang.api.ErlangPunctuator.DIV;
import static org.sonar.erlang.api.ErlangPunctuator.DOT;
import static org.sonar.erlang.api.ErlangPunctuator.EQUAL;
import static org.sonar.erlang.api.ErlangPunctuator.EQUAL2;
import static org.sonar.erlang.api.ErlangPunctuator.EXCLAMATION;
import static org.sonar.erlang.api.ErlangPunctuator.GE;
import static org.sonar.erlang.api.ErlangPunctuator.GT;
import static org.sonar.erlang.api.ErlangPunctuator.LBRACKET;
import static org.sonar.erlang.api.ErlangPunctuator.LCURLYBRACE;
import static org.sonar.erlang.api.ErlangPunctuator.LE;
import static org.sonar.erlang.api.ErlangPunctuator.LISTCOMP;
import static org.sonar.erlang.api.ErlangPunctuator.LPARENTHESIS;
import static org.sonar.erlang.api.ErlangPunctuator.LT;
import static org.sonar.erlang.api.ErlangPunctuator.MATCHOP;
import static org.sonar.erlang.api.ErlangPunctuator.MINUS;
import static org.sonar.erlang.api.ErlangPunctuator.MINUSMINUS;
import static org.sonar.erlang.api.ErlangPunctuator.NOTEQUAL;
import static org.sonar.erlang.api.ErlangPunctuator.NOTEQUAL2;
import static org.sonar.erlang.api.ErlangPunctuator.NUMBERSIGN;
import static org.sonar.erlang.api.ErlangPunctuator.PIPE;
import static org.sonar.erlang.api.ErlangPunctuator.PLUS;
import static org.sonar.erlang.api.ErlangPunctuator.PLUSPLUS;
import static org.sonar.erlang.api.ErlangPunctuator.QUESTIONMARK;
import static org.sonar.erlang.api.ErlangPunctuator.RBRACKET;
import static org.sonar.erlang.api.ErlangPunctuator.RCURLYBRACE;
import static org.sonar.erlang.api.ErlangPunctuator.RPARENTHESIS;
import static org.sonar.erlang.api.ErlangPunctuator.SEMI;
import static org.sonar.erlang.api.ErlangPunctuator.STAR;
import static org.sonar.erlang.api.ErlangTokenType.NUMERIC_LITERAL;

import com.sonar.sslr.impl.matcher.GrammarFunctions;

import static com.sonar.sslr.api.GenericTokenType.EOF;
import static com.sonar.sslr.api.GenericTokenType.IDENTIFIER;
import static com.sonar.sslr.api.GenericTokenType.LITERAL;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Predicate.next;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.and;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.firstOf;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.o2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.one2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.opt;

public class ErlangGrammarImpl extends ErlangGrammar {

  public ErlangGrammarImpl() {
    expressions();
    statements();
    module();
    functions();
    GrammarFunctions.enableMemoizationOfMatchesForAllRules(this);
  }

  private void module() {
    module.is(one2n(
        // TODO: does the -module mandatory? maybe we should move it one level
        // up
        firstOf(moduleHeadAttr, and(macroLiteral, DOT), functionDeclaration)), EOF);

    moduleHeadAttr.is(firstOf(moduleAttr, fileAttr, exportAttr, compileAttr, defineAttr,
        importAttr, typeSpec, spec, recordAttr, flowControlAttr, genericAttr));

    recordAttr.is(MINUS, "record", LPARENTHESIS, IDENTIFIER, COMMA, LCURLYBRACE, opt(and(
        recordField, opt(MATCHOP, recordField)), o2n(firstOf(COMMA, PIPE), and(recordField,
        opt(MATCHOP, recordField)))), RCURLYBRACE, RPARENTHESIS, DOT);

    recordField.is(firstOf(and(firstOf(LCURLYBRACE, LBRACKET), recordField, o2n(COMMA,
        recordField), firstOf(RCURLYBRACE, RBRACKET)),
        and(firstOf(specFun, callExpression))), opt(COLON, COLON, recordField));

    flowControlAttr.is(firstOf(ifdefAttr, ifndefAttr), one2n(firstOf(moduleHeadAttr,
        functionDeclaration)), opt(elseAttr, one2n(firstOf(moduleHeadAttr,
        functionDeclaration))), endifAttr);

    ifdefAttr.is(MINUS, "ifdef", LPARENTHESIS, IDENTIFIER, RPARENTHESIS, DOT);

    ifndefAttr.is(MINUS, "ifndef", LPARENTHESIS, IDENTIFIER, RPARENTHESIS, DOT);

    elseAttr.is(MINUS, "else", DOT);

    endifAttr.is(MINUS, "endif", DOT);

    moduleAttr.is(MINUS, "module", LPARENTHESIS, IDENTIFIER, RPARENTHESIS, DOT);
    exportAttr.is(MINUS, "export", LPARENTHESIS, funcExport, RPARENTHESIS, DOT);
    compileAttr.is(MINUS, "compile", LPARENTHESIS, primaryExpression, RPARENTHESIS, DOT);

    defineAttr.is(MINUS, "define", LPARENTHESIS, firstOf(and(IDENTIFIER, COMMA,
        assignmentExpression), and(funcDecl, COMMA, statement)), RPARENTHESIS, DOT);

    importAttr.is(MINUS, "import", LPARENTHESIS, firstOf(macroLiteral, IDENTIFIER), COMMA,
        LBRACKET, funcArity, o2n(COMMA, funcArity), RBRACKET, RPARENTHESIS, DOT);

    fileAttr.is(MINUS, "file", LPARENTHESIS, primaryExpression, COMMA, primaryExpression,
        RPARENTHESIS, DOT);

    genericAttr.is(MINUS, firstOf("behaviour", "vsn", "on_load", "include", "file",
        "ignore_xref", "include_lib", "author", "export_type", "deprecated", "asn1_info"),
        LPARENTHESIS, firstOf(funcArity, primaryExpression), RPARENTHESIS, DOT);
    // TODO: is it possible to have something like: -export().?
    funcExport.is(firstOf(and(LBRACKET, o2n(funcArity, o2n(COMMA, funcArity)), RBRACKET),
        funcArity));
  }

  private void functions() {
    spec.is(ErlangPunctuator.MINUS, firstOf("spec", "callback"), opt(LPARENTHESIS), opt(
        IDENTIFIER, COLON), IDENTIFIER, opt(DIV, NUMERIC_LITERAL), opt(COLON, COLON),
        funcSpec, o2n(SEMI, funcSpec), opt(RPARENTHESIS), DOT);

    typeSpec.is(MINUS, firstOf("type", "opaque"), opt(LPARENTHESIS), funcDecl, firstOf(and(
        COLON, COLON, specType), and(ARROW, funcDecl)), opt(RPARENTHESIS), DOT);

    funcSpec.is(LPARENTHESIS, opt(specType), RPARENTHESIS, ARROW, specType, opt(WHEN, specType));

    specType.is(one2n(specTypeDef, o2n(firstOf(PIPE, COMMA), specTypeDef)));

    specTypeDef.is(firstOf(and(firstOf(LCURLYBRACE, LBRACKET), specTypeDef, firstOf(
        RCURLYBRACE, RBRACKET)), specSub), o2n(firstOf(COMMA, PIPE), firstOf(and(firstOf(
        LCURLYBRACE, LBRACKET), specTypeDef, firstOf(RCURLYBRACE, RBRACKET)), specSub)));

    specSub.is(firstOf(
        // things in ()
        and(LPARENTHESIS, specTypeDef, RPARENTHESIS),
        // workaround for fun like expression:fun(), fun((id())-> error
        // | ok)
        and(specFun),
        // something like: list(A | B)
        and(IDENTIFIER, LPARENTHESIS, callExpression, one2n(PIPE, callExpression),
            RPARENTHESIS),
        // and: Mega::giga()
        and(firstOf(funcArity, IDENTIFIER), COLON, COLON, specTypeDef),
        // and for records
        and(NUMBERSIGN, IDENTIFIER, specTypeDef),
        // and things like: 1..255
        and(primaryExpression, DOT, DOT, primaryExpression),
        // or just simple ...
        and(DOT, DOT, DOT),
        // and simple function call
        and(opt(IDENTIFIER, COLON), IDENTIFIER, LPARENTHESIS, opt(specTypeDef),
            RPARENTHESIS),
        // and everything other
        callExpression));

    specFun.is(FUN, LPARENTHESIS, opt(LPARENTHESIS, opt(specTypeDef), RPARENTHESIS), opt(ARROW,
        specTypeDef), RPARENTHESIS);

    functionDeclaration.is(functionClause, o2n(SEMI, functionClause),

        DOT);
    functionClause.is(clauseHead, ARROW, clauseBody);
    clauseHead.is(funcDecl, opt(guardSequenceStart));
    clauseBody.is(statements);

    funcArity.is(opt(literal, ErlangPunctuator.COLON), literal, ErlangPunctuator.DIV, literal);

    funcDecl.is(IDENTIFIER, arguments);
  }

  private void expressions() {
    literal.is(firstOf(IDENTIFIER, NUMERIC_LITERAL,
        // handle string concetanation ("..."\n[\r\t]"..." is one literal as
        // well this:
        // "asasd" ?MACRO "asdasd"
        and(LITERAL, o2n(firstOf(LITERAL, macroLiteral)))));
    primaryExpression.is(firstOf(literal, listLiteral, tupleLiteral, binaryLiteral, and(
        LPARENTHESIS, expression, RPARENTHESIS)));

    listLiteral.is(LBRACKET, opt(firstOf(and(assignmentExpression, LISTCOMP, qualifier, o2n(
        COMMA, qualifier)), and(assignmentExpression, o2n(firstOf(COMMA,
        assignmentExpression)), opt(PIPE, assignmentExpression)))), RBRACKET);
    qualifier.is(firstOf(and(assignmentExpression, ARROWBACK, expression), expression));
    recordLiteral.is(opt(primaryExpression), one2n(recordLiteralHead), opt(LCURLYBRACE, opt(
        assignmentExpression, o2n(COMMA, assignmentExpression)), RCURLYBRACE));
    recordLiteralHead.is(NUMBERSIGN, IDENTIFIER, o2n(DOT, IDENTIFIER));

    macroLiteral.is(QUESTIONMARK, IDENTIFIER, opt(arguments));
    tupleLiteral.is(LCURLYBRACE, o2n(firstOf(COMMA, expression)), RCURLYBRACE);
    binaryLiteral.is(BINSTART, firstOf(and(and(assignmentExpression, LISTCOMP,
        one2n(binaryQualifier)), o2n(firstOf(COMMA, assignmentExpression))), o2n(firstOf(
        COMMA, binaryElement))), BINEND);
    binaryQualifier.is(firstOf(
        and(binaryLiteral, ErlangPunctuator.DOUBLEARROWBACK, expression), and(
            primaryExpression, ARROWBACK, expression, o2n(COMMA, expression)

        )));

    binaryElement.is(firstOf(and(expression, opt(COLON, firstOf(NUMERIC_LITERAL, IDENTIFIER,
        macroLiteral)), opt(ErlangPunctuator.DIV,
        /*
         * Hack for things like: 1024:32/little-float-dafaq
         */
        firstOf(NUMERIC_LITERAL, and(IDENTIFIER, one2n(MINUS, IDENTIFIER)), IDENTIFIER)))));
    memberExpression.is(
        firstOf(recordLiteral, macroLiteral, ifExpression, funExpression, caseExpression,
            tryExpression, receiveExpression, blockExpression, primaryExpression))
        .skipIfOneChild();
    /**
     * It can be a record ref (originaly a.b['a']) as well
     */
    callExpression.is(
        firstOf(and(opt(memberExpression, COLON), memberExpression, arguments),
            memberExpression)).skipIfOneChild();

    arguments.is(LPARENTHESIS, opt(assignmentExpression, o2n(COMMA, assignmentExpression)),
        RPARENTHESIS);
    unaryExpression.is(firstOf(
        // handle things like: -12, -A, -func(A), -(6+3)
        and(opt(MINUS), callExpression), and(NOT, unaryExpression))).skipIfOneChild();
    otherArithmeticExpression.is(unaryExpression,
        o2n(firstOf(BNOT, ErlangKeyword.DIV, REM), unaryExpression)).skipIfOneChild();
    multiplicativeExpression.is(otherArithmeticExpression,
        o2n(firstOf(STAR, DIV), otherArithmeticExpression)).skipIfOneChild();
    additiveExpression.is(multiplicativeExpression,
        o2n(firstOf(PLUS, MINUS), multiplicativeExpression)).skipIfOneChild();

    shiftExpression.is(additiveExpression, o2n(firstOf(BSL, BSR), additiveExpression))
        .skipIfOneChild();

    relationalExpression.is(shiftExpression, o2n(firstOf(LT, GT, LE, GE), shiftExpression))
        .skipIfOneChild();

    equalityExpression.is(relationalExpression,
        o2n(firstOf(EQUAL, NOTEQUAL, EQUAL2, NOTEQUAL2), relationalExpression))
        .skipIfOneChild();

    bitwiseAndExpression.is(equalityExpression, o2n(BAND, equalityExpression)).skipIfOneChild();

    bitwiseXorExpression.is(bitwiseAndExpression, o2n(BXOR, bitwiseAndExpression))
        .skipIfOneChild();

    bitwiseOrExpression.is(bitwiseXorExpression, o2n(BOR, bitwiseXorExpression))
        .skipIfOneChild();

    logicalAndExpression.is(bitwiseOrExpression, o2n(AND, bitwiseOrExpression))
        .skipIfOneChild();

    logicalOrExpression.is(logicalAndExpression, o2n(OR, logicalAndExpression))
        .skipIfOneChild();

    logicalXorExpression.is(logicalOrExpression, o2n(XOR, logicalOrExpression))
        .skipIfOneChild();

    shortCircuitOrElseExpression.is(logicalXorExpression, o2n(ORELSE, logicalXorExpression))
        .skipIfOneChild();

    shortCircuitAndAlsoExpression.is(shortCircuitOrElseExpression,
        o2n(ANDALSO, shortCircuitOrElseExpression)).skipIfOneChild();

    listOperationExpression.is(shortCircuitAndAlsoExpression,
        o2n(firstOf(PLUSPLUS, MINUSMINUS), shortCircuitAndAlsoExpression)).skipIfOneChild();

    assignmentExpression.is(
        firstOf(and(listOperationExpression, MATCHOP, assignmentExpression),
            listOperationExpression)).skipIfOneChild();

    expression.is(opt(CATCH), assignmentExpression);

    funExpression.is(ErlangKeyword.FUN, firstOf(and(opt(memberExpression, COLON), funcArity),
        and(functionDeclarationsNoName, END)), opt(arguments));
    functionDeclarationsNoName.is(functionDeclarationNoName, o2n(SEMI,
        functionDeclarationNoName));
    functionDeclarationNoName.is(arguments, opt(guardSequenceStart), ARROW, statements);

    caseExpression.is(CASE, expression, OF, patternStatements, END);

    ifExpression.is(IF, branchExps, END);

    tryExpression.is(TRY, statements, opt(OF, patternStatements), firstOf(and(catchExpression,
        afterExpression), catchExpression, afterExpression), END);

    afterExpression.is(AFTER, statements);

    catchExpression.is(CATCH, catchPatternStatements);

    receiveExpression.is(RECEIVE, firstOf(and(patternStatements, opt(AFTER, expression, ARROW,
        statements)), and(AFTER, expression, ARROW, statements)), END);

    blockExpression.is(BEGIN, statements, END);
  }

  /**
   * A.4 Statement
   **/
  private void statements() {
    eos.is(firstOf(COMMA, next(EOF), next(DOT)));
    statement
        .is(firstOf(sendStatement, expressionStatement, receiveExpression, blockExpression));
    statements.is(statement, o2n(COMMA, statement));
    expressionStatement.is(expression);

    patternStatements.is(patternStatement, o2n(SEMI, patternStatement));

    catchPatternStatements.is(catchPatternStatement, o2n(SEMI, catchPatternStatement));

    patternStatement.is(pattern, opt(guardSequenceStart), ARROW, statements);

    catchPatternStatement.is(catchPattern, opt(guardSequenceStart), ARROW, statements);

    pattern.is(assignmentExpression);

    catchPattern.is(opt(IDENTIFIER, COLON), assignmentExpression);

    branchExps.is(branchExp, o2n(ErlangPunctuator.SEMI, branchExp));

    branchExp.is(guardSequence, ErlangPunctuator.ARROW, statements);

    guardSequenceStart.is(ErlangKeyword.WHEN, guardSequence);

    guardSequence.is(guard, o2n(ErlangPunctuator.SEMI, guard));
    guard.is(guardExpression, o2n(ErlangPunctuator.COMMA, guardExpression));
    guardExpression.is(expression);

    sendStatement.is(expression, EXCLAMATION, expression);

  }
}
