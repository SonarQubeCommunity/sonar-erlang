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

import static org.sonar.sslr.parser.GrammarOperators.commentTrivia;
import static org.sonar.sslr.parser.GrammarOperators.endOfInput;
import static org.sonar.sslr.parser.GrammarOperators.firstOf;
import static org.sonar.sslr.parser.GrammarOperators.nextNot;
import static org.sonar.sslr.parser.GrammarOperators.oneOrMore;
import static org.sonar.sslr.parser.GrammarOperators.optional;
import static org.sonar.sslr.parser.GrammarOperators.regexp;
import static org.sonar.sslr.parser.GrammarOperators.sequence;
import static org.sonar.sslr.parser.GrammarOperators.skippedTrivia;
import static org.sonar.sslr.parser.GrammarOperators.token;
import static org.sonar.sslr.parser.GrammarOperators.zeroOrMore;

public class ErlangGrammarImpl extends ErlangGrammar {

  public static final String EXP = "([Ee][-]?+[0-9_]++)";
  public static final String ESCAPE_SEQUENCE = "(\\$\\\\b)|(\\$\\\\d)|(\\$\\\\e)|(\\$\\\\f)|(\\$\\\\n)|(\\$\\\\r)|(\\$\\\\s)|(\\$\\\\t)|(\\$\\\\v)|(\\$\\\\')|(\\$\\\\\")|(\\$\\\\\\\\)"
    + "|(\\$\\\\\\^[A-Za-z])"
    + "|(\\$\\\\x\\{[A-F0-9]+\\})"
    + "|(\\$\\\\x[A-F0-9]{1,2})"
    + "|(\\$\\\\[0-7]{1,3})";

  public static final String NUMERIC_LITERAL = "(?:"
    + "[0-9]++\\.([0-9]++)" + EXP + "?"
    + "|[0-9]++\\#([0-9A-Fa-f]++)?+"
    + "|[0-9]++"
    + "|" + ESCAPE_SEQUENCE
    + "|\\$[\\x00-\\x7F]"
    + ")";

  public static final String LITERAL = "(?:"
    + "\"([^\"\\\\]*+(\\\\[\\s\\S])?+)*+\")";

  public static final String COMMENT = "(?:"
    + "%[^\\n\\r]*+)";

  public static final String WHITESPACE = "[\\n\\r\\t\\u000B\\f\\u0020\\u00A0\\uFEFF\\p{Zs}]";

  public static final String IDENTIFIER = "('[^'\n\r]*')"
    + "|^(?!\\$)(\\p{javaJavaIdentifierStart}++[\\p{javaJavaIdentifierPart}@]*+)";

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
        regexp(IDENTIFIER), spacing);

    numericLiteral.is(
        regexp(NUMERIC_LITERAL), spacing);

    stringLiteral.is(
        regexp(LITERAL), spacing);

    /*
     * TODO use the keywords directly
     */
    keyword.is(firstOf(
        "after",
        "andalso",
        "and",
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
        "orelse",
        "or",
        "query",
        "receive",
        "rem",
        "try",
        "when",
        "xor"), nextNot(letterOrDigit));

    letterOrDigit.is(regexp("\\p{javaJavaIdentifierPart}"));

    spacing.is(
        skippedTrivia(regexp(WHITESPACE + "*+")),
        zeroOrMore(commentTrivia(regexp(COMMENT)), skippedTrivia(regexp(WHITESPACE + "*+")))
        ).skip();
  }

  private void punctuators() {
    arrow.is(punctuator("->"));
    arrowback.is(punctuator("<-"));
    doublearrowback.is(punctuator("<="));
    lcurlybrace.is(punctuator("{"));
    rcurlybrace.is(punctuator("}"));
    lparenthesis.is(punctuator("("));
    rparenthesis.is(punctuator(")"));
    lbracket.is(punctuator("["));
    rbracket.is(punctuator("]"));
    dot.is(punctuator("."));
    semi.is(punctuator(";"));
    comma.is(punctuator(","));
    colon.is(punctuator(":"));
    matchop.is(punctuator("=", nextNot(firstOf("=", "<", ":", "/"))));
    plus.is(punctuator("+", nextNot("+")));
    minus.is(punctuator("-", nextNot(firstOf(">", "-"))));
    star.is(punctuator("*"));
    div.is(punctuator("/", nextNot("=")));
    lt.is(punctuator("<", nextNot(firstOf("=", "<"))));
    gt.is(punctuator(">", nextNot(firstOf("=", ">"))));
    le.is(punctuator("=<"));
    ge.is(punctuator(">="));
    equal.is(punctuator("=="));
    notequal.is(punctuator("/="));
    equal2.is(punctuator("=:="));
    notequal2.is(punctuator("=/="));
    binstart.is(punctuator("<<"));
    binend.is(punctuator(">>"));
    listcomp.is(punctuator("||"));
    pipe.is(punctuator("|", nextNot("|")));
    dollar.is(punctuator("$"));
    apostrophe.is(punctuator("'"));
    plusplus.is(punctuator("++"));
    minusminus.is(punctuator("--"));
    numbersign.is(punctuator("#"));
    exclamation.is(punctuator("!"));
    questionmark.is(punctuator("?"));
  }

  private void keywords() {
    afterKeyword.is(keyword("after"));
    andKeyword.is(keyword("and"));
    andalsoKeyword.is(keyword("andalso"));
    bandKeyword.is(keyword("band"));
    beginKeyword.is(keyword("begin"));
    bnotKeyword.is(keyword("bnot"));
    borKeyword.is(keyword("bor"));
    bslKeyword.is(keyword("bsl"));
    bsrKeyword.is(keyword("bsr"));
    bxorKeyword.is(keyword("bxor"));
    caseKeyword.is(keyword("case"));
    catchKeyword.is(keyword("catch"));
    condKeyword.is(keyword("cond"));
    divKeyword.is(keyword("div"));
    endKeyword.is(keyword("end"));
    funKeyword.is(keyword("fun"));
    ifKeyword.is(keyword("if"));
    letKeyword.is(keyword("let"));
    notKeyword.is(keyword("not"));
    ofKeyword.is(keyword("of"));
    orKeyword.is(keyword("or"));
    orelseKeyword.is(keyword("orelse"));
    queryKeyword.is(keyword("query"));
    receiveKeyword.is(keyword("receive"));
    remKeyword.is(keyword("rem"));
    tryKeyword.is(keyword("try"));
    whenKeyword.is(keyword("when"));
    xorKeyword.is(keyword("xor"));
  }

  private void module() {
    module.is(spacing, optional(moduleElements), eof);
    moduleElements.is(oneOrMore(
        moduleElement
        ));

    moduleElement.is(firstOf(moduleHeadAttr, sequence(macroLiteral, dot), functionDeclaration)).skipIfOneChild();

    moduleHeadAttr.is(firstOf(moduleAttr, fileAttr, exportAttr, compileAttr, defineAttr,
        importAttr, typeSpec, spec, recordAttr, flowControlAttr, behaviourAttr, genericAttr, anyAttr)).skipIfOneChild();

    recordAttr.is(minus, semiKeyword("record"), lparenthesis, identifier, comma, lcurlybrace, optional(sequence(
        recordField, optional(matchop, recordField)), zeroOrMore(firstOf(comma, pipe), sequence(recordField,
        optional(matchop, recordField)))), rcurlybrace, rparenthesis, dot);

    recordField.is(firstOf(sequence(firstOf(lcurlybrace, lbracket), recordField, zeroOrMore(comma,
        recordField), firstOf(rcurlybrace, rbracket)),
        sequence(firstOf(specFun, callExpression))), optional(colon, colon, recordField));

    flowControlAttr.is(firstOf(ifdefAttr, ifndefAttr), zeroOrMore(firstOf(moduleHeadAttr,
        functionDeclaration)), optional(elseAttr, zeroOrMore(firstOf(moduleHeadAttr,
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

    anyAttr.is(minus, identifier, lparenthesis, primaryExpression, rparenthesis, dot);

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
    literal.is(oneOrMore(firstOf(stringLiteral, numericLiteral, identifier, macroLiteral)));
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
        sequence(optional(minus), callExpression), sequence(notKeyword, callExpression))).skipIfOneChild();
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
    return sequence(value, spacing);
  }

  private Object punctuator(String value, Object element) {
    return sequence(value, element, spacing);
  }

  private Object keyword(String value) {
    return sequence(value, nextNot(letterOrDigit), spacing);
  }

  private Object semiKeyword(String value) {
    return sequence(value, nextNot(letterOrDigit), spacing);
  }
}
