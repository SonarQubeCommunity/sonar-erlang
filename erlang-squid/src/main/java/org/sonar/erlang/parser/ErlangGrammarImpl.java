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
package org.sonar.erlang.parser;

import com.sonar.sslr.api.GenericTokenType;
import org.sonar.sslr.grammar.GrammarRuleKey;
import org.sonar.sslr.grammar.LexerlessGrammarBuilder;
import org.sonar.sslr.parser.LexerlessGrammar;

public enum ErlangGrammarImpl implements GrammarRuleKey {

  eof,

  stringLiteral,
  regularExpressionLiteral,

  numericLiteral,
  identifier,

  keyword,
  letterOrDigit,

  spacing,

  afterKeyword,
  andKeyword,
  andalsoKeyword,
  bandKeyword,
  beginKeyword,
  bnotKeyword,
  borKeyword,
  bslKeyword,
  bsrKeyword,
  bxorKeyword,
  caseKeyword,
  catchKeyword,
  condKeyword,
  divKeyword,
  endKeyword,
  funKeyword,
  ifKeyword,
  letKeyword,
  notKeyword,
  ofKeyword,
  orKeyword,
  orelseKeyword,
  queryKeyword,
  receiveKeyword,
  remKeyword,
  tryKeyword,
  whenKeyword,
  xorKeyword,

  // Punctuators
  arrow,
  arrowback,
  doublearrowback,
  lcurlybrace,
  rcurlybrace,
  lparenthesis,
  rparenthesis,
  lbracket,
  rbracket,
  dot,
  semi,
  comma,
  colon,
  matchop,
  plus,
  minus,
  star,
  div,
  lt,
  gt,
  le,
  ge,
  equal,
  notequal,
  equal2,
  notequal2,
  binstart,
  binend,
  listcomp,
  pipe,
  dollar,
  apostrophe,
  plusplus,
  minusminus,
  numbersign,
  exclamation,
  questionmark,
  mapA,
  mapU,

  module,
  functionDeclaration,
  moduleAttr,
  exportAttr,
  compileAttr,
  defineAttr,
  typeSpec,
  genericAttr,
  anyAttr,
  funcExport,
  expression,
  funcArity,
  functionClause,
  clauseHead,
  guardSequenceStart,
  funcDecl,
  clauseBody,
  pattern,
  literal,
  primaryExpression,
  listLiteral,
  tupleLiteral,
  binaryLiteral,
  assignmentExpression,
  memberExpression,
  funExpression,
  arguments,
  unaryExpression,
  multiplicativeExpression,
  additiveExpression,
  shiftExpression,
  relationalExpression,
  equalityExpression,
  bitwiseAndExpression,
  bitwiseXorExpression,
  bitwiseOrExpression,
  logicalAndExpression,
  logicalOrExpression,
  leftHandSideExpression,
  callExpression,
  callExpressionFirstMember,
  callExpressionSecondMember,
  qualifier,
  listOperationExpression,
  logicalXorExpression,
  shortCircuitOrElseExpression,
  shortCircuitAndAlsoExpression,
  binaryElement,
  binaryQualifier,
  expressionStatement,
  statement,
  ifExpression,
  caseExpression,
  receiveExpression,
  tryExpression,
  branchExps,
  branchExp,
  guardSequence,
  guard,
  guardExpression,
  functionDeclarationsNoName,
  functionDeclarationNoName,
  patternStatements,
  patternStatement,
  statements,
  sendExpression,
  catchExpression,
  afterExpression,
  catchPattern,
  catchPatternStatement,
  catchPatternStatements,
  blockExpression,
  macroLiteral,
  otherArithmeticExpression,
  ifdefAttr,
  ifndefAttr,
  elseAttr,
  endifAttr,
  flowControlAttr,
  recordAttr,
  spec,
  moduleHeadAttr,
  importAttr,
  recordField,
  fileAttr,
  behaviourAttr,
  moduleElements,
  moduleElement,
  atom,
  recordCreate,
  recordAccess,
  macroLiteralSimple,
  macroLiteralFunction,
  macroLiteralVarName,
  stringLiterals,
  stringConcatenation,
  guardedPattern, atomOrIdentifier, moduleAttrTags, mapCreate, mapUpdate, mapCreateUpdate, map;

  public static final String EXP = "([Ee][-]?+[0-9_]++)";
  public static final String ESCAPE_SEQUENCE =
    "(\\$\\\\b)|(\\$\\\\d)|(\\$\\\\e)|(\\$\\\\f)|(\\$\\\\n)|(\\$\\\\r)|(\\$\\\\s)|(\\$\\\\t)|(\\$\\\\v)|(\\$\\\\')|(\\$\\\\\")|(\\$\\\\\\\\)"
      + "|(\\$\\\\\\^[A-Za-z])"
      + "|(\\$\\\\x\\{[A-F0-9]+\\})"
      + "|(\\$\\\\x[A-F0-9]{1,2})"
      + "|(\\$\\\\[0-7]{1,3})";

  public static final String NUMERIC_LITERAL = "(?:"
    + "[0-9]++\\.([0-9]++)" + EXP + "?"
    + "|[0-9]++\\#([0-9A-Fa-f]++)?+"
    + "|[0-9]++"
    + "|" + ESCAPE_SEQUENCE
    // parsing things like: '$a' or '$\]' '$\n'
    + "|\\$\\\\?[\\x00-\\x7F]"
    + ")";

  public static final String LITERAL = "(?:"
    + "\"([^\"\\\\]*+(\\\\[\\s\\S])?+)*+\")";

  public static final String COMMENT = "(?:"
    + "%[^\\n\\r]*+)";

  public static final String WHITESPACE = "[\\n\\r\\t\\u000B\\f\\u0020\\u00A0\\uFEFF\\p{Zs}]";

  public static final String IDENTIFIER = "^[A-Z_][a-zA-Z0-9_@]*";

  public static final String ATOM = "('[^'\\n\\r]*')|^[a-z][a-zA-Z0-9_@]*";

  // public static final String IDENTIFIER = "('[^'\n\r]*')"
  // + "|^(?!\\$)(\\p{javaJavaIdentifierStart}++[\\p{javaJavaIdentifierPart}@]*+)";

  public static LexerlessGrammar createGrammar() {
    return createGrammarBuilder().build();
  }

  public static LexerlessGrammarBuilder createGrammarBuilder() {
    LexerlessGrammarBuilder b = LexerlessGrammarBuilder.create();
    lexical(b);
    punctuators(b);
    keywords(b);
    expressions(b);
    branchAndGuardExpressions(b);
    statements(b);
    module(b);
    functions(b);

    b.setRootRule(module);

    return b;
  }

  private static void lexical(LexerlessGrammarBuilder b) {
    b.rule(eof).is(b.token(GenericTokenType.EOF, b.endOfInput())).skip();

    // variable name (identifier): Uppercase letter or _ and may contain alphanumeric chars, underscore, and @
    b.rule(identifier).is(
      b.regexp(IDENTIFIER), spacing);

    // atom: lowercase letter and alphanumeric chars, underscore, and @ or anything between ''
    b.rule(atom).is(
      b.nextNot(keyword),
      b.regexp(ATOM), b.zeroOrMore(b.sequence(".", b.regexp(ATOM))), spacing);

    b.rule(numericLiteral).is(
      b.regexp(NUMERIC_LITERAL), spacing);

    // handle string concetanation ("..."\n[\r\t]"..." is one literal as
    // well this:
    // "asasd" ?MACRO "asdasd"
    b.rule(stringLiteral).is(
      b.sequence(b.regexp(LITERAL), spacing));

    /*
     * TODO use the keywords directly
     */
    b.rule(keyword).is(b.firstOf(
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
      "receive",
      "rem",
      "try",
      "when",
      "xor"), b.nextNot(letterOrDigit));

    b.rule(moduleAttrTags).is(b.firstOf(
      "ifdef",
      "ifndef",
      "else",
      "endif",
      "module",
      "export",
      "compile",
      "define",
      "import",
      "file",
      "behaviour",
      "on_load",
      "file",
      "include",
      "ignore_xref",
      "author",
      "include_lib",
      "export_type",
      "deprecated",
      "asn1_info"
      ));

    b.rule(letterOrDigit).is(b.regexp("\\p{javaJavaIdentifierPart}"));

    b.rule(spacing).is(
      b.skippedTrivia(b.regexp(WHITESPACE + "*")),
      b.zeroOrMore(b.commentTrivia(b.regexp(COMMENT)), b.skippedTrivia(b.regexp(WHITESPACE + "*")))
      ).skip();
  }

  private static void punctuators(LexerlessGrammarBuilder b) {
    b.rule(arrow).is(punctuator("->", b));
    b.rule(arrowback).is(punctuator("<-", b));
    b.rule(doublearrowback).is(punctuator("<=", b));
    b.rule(lcurlybrace).is(punctuator("{", b));
    b.rule(rcurlybrace).is(punctuator("}", b));
    b.rule(lparenthesis).is(punctuator("(", b));
    b.rule(rparenthesis).is(punctuator(")", b));
    b.rule(lbracket).is(punctuator("[", b));
    b.rule(rbracket).is(punctuator("]", b));
    b.rule(dot).is(punctuator(".", b));
    b.rule(semi).is(punctuator(";", b));
    b.rule(comma).is(punctuator(",", b));
    b.rule(colon).is(punctuator(":", b));
    b.rule(matchop).is(punctuator("=", b.nextNot(b.firstOf("=", "<", ":", "/")), b));
    b.rule(plus).is(punctuator("+", b.nextNot("+"), b));
    b.rule(minus).is(punctuator("-", b.nextNot(b.firstOf(">", "-")), b));
    b.rule(star).is(punctuator("*", b));
    b.rule(div).is(punctuator("/", b.nextNot("="), b));
    b.rule(lt).is(punctuator("<", b.nextNot(b.firstOf("=", "<")), b));
    b.rule(gt).is(punctuator(">", b.nextNot(b.firstOf("=", ">")), b));
    b.rule(le).is(punctuator("=<", b));
    b.rule(ge).is(punctuator(">=", b));
    b.rule(equal).is(punctuator("==", b));
    b.rule(notequal).is(punctuator("/=", b));
    b.rule(equal2).is(punctuator("=:=", b));
    b.rule(notequal2).is(punctuator("=/=", b));
    b.rule(binstart).is(punctuator("<<", b));
    b.rule(binend).is(punctuator(">>", b));
    b.rule(listcomp).is(punctuator("||", b));
    b.rule(pipe).is(punctuator("|", b.nextNot("|"), b));
    b.rule(dollar).is(punctuator("$", b));
    b.rule(apostrophe).is(punctuator("'", b));
    b.rule(plusplus).is(punctuator("++", b));
    b.rule(minusminus).is(punctuator("--", b));
    b.rule(numbersign).is(punctuator("#", b));
    b.rule(exclamation).is(punctuator("!", b));
    b.rule(questionmark).is(punctuator("?", b));
    b.rule(mapA).is(punctuator("=>", b));
    b.rule(mapU).is(punctuator(":=", b));
  }

  private static void keywords(LexerlessGrammarBuilder b) {
    b.rule(afterKeyword).is(keyword("after", b));
    b.rule(andKeyword).is(keyword("and", b));
    b.rule(andalsoKeyword).is(keyword("andalso", b));
    b.rule(bandKeyword).is(keyword("band", b));
    b.rule(beginKeyword).is(keyword("begin", b));
    b.rule(bnotKeyword).is(keyword("bnot", b));
    b.rule(borKeyword).is(keyword("bor", b));
    b.rule(bslKeyword).is(keyword("bsl", b));
    b.rule(bsrKeyword).is(keyword("bsr", b));
    b.rule(bxorKeyword).is(keyword("bxor", b));
    b.rule(caseKeyword).is(keyword("case", b));
    b.rule(catchKeyword).is(keyword("catch", b));
    b.rule(condKeyword).is(keyword("cond", b));
    b.rule(divKeyword).is(keyword("div", b));
    b.rule(endKeyword).is(keyword("end", b));
    b.rule(funKeyword).is(keyword("fun", b));
    b.rule(ifKeyword).is(keyword("if", b));
    b.rule(letKeyword).is(keyword("let", b));
    b.rule(notKeyword).is(keyword("not", b));
    b.rule(ofKeyword).is(keyword("of", b));
    b.rule(orKeyword).is(keyword("or", b));
    b.rule(orelseKeyword).is(keyword("orelse", b));
    b.rule(queryKeyword).is(keyword("query", b));
    b.rule(receiveKeyword).is(keyword("receive", b));
    b.rule(remKeyword).is(keyword("rem", b));
    b.rule(tryKeyword).is(keyword("try", b));
    b.rule(whenKeyword).is(keyword("when", b));
    b.rule(xorKeyword).is(keyword("xor", b));
  }

  private static void module(LexerlessGrammarBuilder b) {
    b.rule(module).is(spacing, b.optional(moduleElements), eof);
    b.rule(moduleElements).is(b.oneOrMore(
      moduleElement
      ));

    b.rule(moduleElement).is(b.firstOf(moduleHeadAttr, b.sequence(macroLiteral, dot), functionDeclaration)).skipIfOneChild();

    b.rule(moduleHeadAttr).is(b.firstOf(moduleAttr, fileAttr, exportAttr, compileAttr, defineAttr,
      importAttr, typeSpec, spec, recordAttr, flowControlAttr, behaviourAttr, genericAttr, anyAttr)).skipIfOneChild();

    b.rule(recordAttr).is(minus, semiKeyword("record", b),
      lparenthesis,
      b.zeroOrMore(
        b.nextNot(b.sequence(rparenthesis, spacing, dot)),
        b.regexp("."), spacing),
      rparenthesis, dot);

    b.rule(flowControlAttr).is(
      b.firstOf(ifdefAttr, ifndefAttr),
      b.zeroOrMore(b.firstOf(moduleHeadAttr, functionDeclaration)),
      b.optional(elseAttr,
        b.zeroOrMore(b.firstOf(moduleHeadAttr, functionDeclaration))),
      endifAttr);

    b.rule(ifdefAttr).is(minus, semiKeyword("ifdef", b), lparenthesis, atomOrIdentifier, rparenthesis, dot);

    b.rule(ifndefAttr).is(minus, semiKeyword("ifndef", b), lparenthesis, atomOrIdentifier, rparenthesis, dot);

    b.rule(elseAttr).is(minus, semiKeyword("else", b), dot);

    b.rule(endifAttr).is(minus, semiKeyword("endif", b), dot);

    b.rule(moduleAttr).is(minus, semiKeyword("module", b), lparenthesis, atom, rparenthesis, dot);
    b.rule(exportAttr).is(minus, semiKeyword("export", b), lparenthesis, funcExport, rparenthesis, dot);
    b.rule(compileAttr).is(minus, semiKeyword("compile", b), lparenthesis, primaryExpression, rparenthesis, dot);

    b.rule(defineAttr).is(minus, semiKeyword("define", b),
      lparenthesis,
      b.sequence(
        b.firstOf(
          funcDecl,
          primaryExpression
        ),
        comma,
        b.zeroOrMore(
          b.nextNot(b.sequence(rparenthesis, spacing, dot)),
          b.regexp("."), spacing)),
      rparenthesis, dot);

    b.rule(importAttr).is(minus, semiKeyword("import", b), lparenthesis, b.firstOf(macroLiteral, atom), comma,
      lbracket, funcArity, b.zeroOrMore(comma, funcArity), rbracket, rparenthesis, dot);

    b.rule(fileAttr).is(minus, semiKeyword("file", b), lparenthesis, primaryExpression, comma, primaryExpression,
      rparenthesis, dot);

    b.rule(behaviourAttr).is(minus, semiKeyword("behaviour", b), lparenthesis, b.firstOf(macroLiteral, atom), rparenthesis, dot);

    b.rule(genericAttr).is(
      minus,
      b.firstOf(
        semiKeyword("vsn", b),
        semiKeyword("on_load", b),
        semiKeyword("include", b),
        semiKeyword("file", b),
        semiKeyword("ignore_xref", b),
        semiKeyword("include_lib", b),
        semiKeyword("author", b),
        semiKeyword("export_type", b),
        semiKeyword("deprecated", b),
        semiKeyword("asn1_info", b),
        semiKeyword("export_types", b)),
      lparenthesis, b.firstOf(funcArity, primaryExpression), rparenthesis, dot);

    b.rule(anyAttr).is(minus, b.sequence(b.nextNot(moduleAttrTags), atom), lparenthesis, primaryExpression, rparenthesis, dot);

    // TODO: is it possible to have something like: -export().?
    b.rule(funcExport).is(lbracket, b.zeroOrMore(funcArity, b.zeroOrMore(comma, funcArity)), rbracket);
  }

  private static void functions(LexerlessGrammarBuilder b) {
    b.rule(spec).is(minus, b.firstOf(semiKeyword("spec", b), semiKeyword("callback", b)),
      b.zeroOrMore(b.firstOf(b.regexp("\\.(\\.+|.)"), b.regexp("[^\\.]")), spacing), dot);

    b.rule(typeSpec).is(minus, b.firstOf(semiKeyword("type", b), semiKeyword("opaque", b)),
      b.zeroOrMore(b.firstOf(b.regexp("\\.(\\.+|.)"), b.regexp("[^\\.]")), spacing), dot);

    b.rule(functionDeclaration).is(functionClause, b.zeroOrMore(semi, functionClause), dot);
    b.rule(functionClause).is(clauseHead, arrow, clauseBody);
    b.rule(clauseHead).is(funcDecl, b.optional(guardSequenceStart));
    b.rule(clauseBody).is(statements);

    b.rule(funcArity).is(b.optional(literal, colon), literal, div, literal);

    b.rule(funcDecl).is(literal, arguments);
  }

  private static void expressions(LexerlessGrammarBuilder b) {
    b.rule(literal).is(b.firstOf(numericLiteral, atomOrIdentifier, macroLiteral));
    b.rule(atomOrIdentifier).is(b.firstOf(identifier, atom)).skip();

    b.rule(primaryExpression).is(
      b.firstOf(
        b.sequence(lparenthesis, expression, rparenthesis),
        literal,
        stringLiteral,
        listLiteral,
        tupleLiteral,
        binaryLiteral));

    b.rule(stringLiterals).is(
      b.firstOf(
        macroLiteralSimple,
        macroLiteralVarName,
        stringLiteral
      )).skip();

    b.rule(stringConcatenation).is(
      b.firstOf(
        b.sequence(stringLiterals, b.oneOrMore(stringLiterals)),
        primaryExpression)
    ).skipIfOneChild();

    b.rule(recordAccess).is(
      stringConcatenation,
      b.zeroOrMore(
        b.firstOf(
          // in defines a record might have an identifier to access a field
          b.sequence(numbersign, primaryExpression, b.optional(".", atomOrIdentifier)),
          b.sequence(macroLiteral, b.optional(".", primaryExpression)))
      )
      ).skipIfOneChild();

    b.rule(recordCreate).is(
      b.firstOf(
        recordAccess,
        b.oneOrMore(numbersign, b.nextNot(lcurlybrace), primaryExpression)),
      b.optional(
        lcurlybrace,
        b.optional(assignmentExpression,
          b.zeroOrMore(comma, assignmentExpression)),
        rcurlybrace
      )
      ).skipIfOneChild();

    b.rule(mapCreate).is(
      numbersign,
      lcurlybrace,
      b.optional(
        mapCreateUpdate,
        b.zeroOrMore(comma, mapCreateUpdate)
      ),
      rcurlybrace
    );

    b.rule(mapCreateUpdate).is(
      expression,
      b.firstOf(mapU, mapA),
      expression
    );

    b.rule(mapUpdate).is(
      stringConcatenation,
      numbersign,
      lcurlybrace,
      mapCreateUpdate,
      b.zeroOrMore(comma, mapCreateUpdate),
      rcurlybrace
    );

    b.rule(map).is(
      b.firstOf(
        mapUpdate,
        mapCreate,
        recordCreate
      )
    ).skipIfOneChild();

    b.rule(guardedPattern).is(map, b.optional(guardSequenceStart)).skipIfOneChild();

    // should be refactored
    b.rule(listLiteral).is(lbracket, b.optional(
      b.firstOf(
        b.sequence(expression, listcomp, qualifier, b.zeroOrMore(comma, qualifier)),
        b.sequence(expression, b.zeroOrMore(b.firstOf(comma, expression)), b.optional(pipe, expression)))),
      rbracket);
    // this does not work
    b.rule(qualifier).is(b.firstOf(b.sequence(expression, arrowback, expression), expression));

    b.rule(macroLiteral).is(
      b.firstOf(
        macroLiteralVarName,
        macroLiteralFunction,
        macroLiteralSimple
        ));

    b.rule(macroLiteralSimple).is(questionmark, atomOrIdentifier);
    b.rule(macroLiteralFunction).is(questionmark, atomOrIdentifier, arguments);
    b.rule(macroLiteralVarName).is(questionmark, questionmark, identifier);

    b.rule(tupleLiteral).is(lcurlybrace, b.zeroOrMore(b.firstOf(comma, expression)), rcurlybrace);
    b.rule(binaryLiteral).is(b.firstOf(lbracket, binstart), b.firstOf(b.sequence(b.sequence(assignmentExpression, listcomp,
      b.oneOrMore(binaryQualifier)), b.zeroOrMore(b.firstOf(comma, assignmentExpression))), b.zeroOrMore(b.firstOf(
      comma, binaryElement))), b.firstOf(binend, rbracket));
    b.rule(binaryQualifier).is(b.firstOf(
      b.sequence(binaryLiteral, doublearrowback, expression), b.sequence(
        primaryExpression, arrowback, expression, b.zeroOrMore(comma, expression)
        )));

    b.rule(binaryElement).is(
      b.sequence(expression,
        b.optional(
          colon,
          b.firstOf(numericLiteral, atom, identifier, macroLiteral,
            b.sequence(lparenthesis, expression, rparenthesis))),
        b.optional(
          div,
          /*
           * Hack for things like: 1024:32/little-float-dafaq
           */
          b.firstOf(
            numericLiteral,
            b.sequence(
              atom,
              b.oneOrMore(minus, atom)),
            atom)
          ),
        // and for things like: Part1:4/big-unsigned-integer-unit:8
        b.optional(colon, numericLiteral)
        )
      );
    b.rule(memberExpression).is(
      b.firstOf(ifExpression, funExpression, caseExpression, tryExpression, receiveExpression, blockExpression, guardedPattern))
      .skipIfOneChild();

    /*
      It can be a record ref (originaly a.b['a']) as well
     */
    b.rule(callExpression).is(
      b.firstOf(
        b.sequence(b.optional(callExpressionFirstMember, colon), callExpressionSecondMember, arguments),
        memberExpression)).skipIfOneChild();
    // TODO Added by Dinesh to get rid of the AstNode.getChild(int) calls in IsTailRecursiveCheck.getArityFromCall(), but should be improved
    b.rule(callExpressionFirstMember).is(memberExpression);
    b.rule(callExpressionSecondMember).is(memberExpression);

    b.rule(arguments).is(lparenthesis, b.optional(expression, b.zeroOrMore(comma, expression)),
      rparenthesis);
    b.rule(unaryExpression).is(b.firstOf(
      // handle things like: -12, -A, -func(A), -(6+3), bnot A
      // TODO why do we have notKeyword and minus here??
      b.sequence(b.optional(b.firstOf(bnotKeyword, minus)), callExpression),
      b.sequence(notKeyword, callExpression))).skipIfOneChild();
    b.rule(otherArithmeticExpression).is(unaryExpression,
      b.zeroOrMore(b.firstOf(divKeyword, remKeyword), unaryExpression)).skipIfOneChild();
    b.rule(multiplicativeExpression).is(otherArithmeticExpression,
      b.zeroOrMore(b.firstOf(star, div), otherArithmeticExpression)).skipIfOneChild();
    b.rule(additiveExpression).is(multiplicativeExpression,
      b.zeroOrMore(b.firstOf(plus, minus), multiplicativeExpression)).skipIfOneChild();

    b.rule(shiftExpression).is(additiveExpression, b.zeroOrMore(b.firstOf(bslKeyword, bsrKeyword), additiveExpression))
      .skipIfOneChild();
    b.rule(relationalExpression).is(shiftExpression, b.zeroOrMore(b.firstOf(lt, gt, le, ge), shiftExpression))
      .skipIfOneChild();

    b.rule(equalityExpression).is(relationalExpression,
      b.zeroOrMore(b.firstOf(equal, notequal, equal2, notequal2), relationalExpression))
      .skipIfOneChild();

    b.rule(bitwiseAndExpression).is(equalityExpression, b.zeroOrMore(bandKeyword, equalityExpression)).skipIfOneChild();

    b.rule(bitwiseXorExpression).is(bitwiseAndExpression, b.zeroOrMore(bxorKeyword, bitwiseAndExpression))
      .skipIfOneChild();

    b.rule(bitwiseOrExpression).is(bitwiseXorExpression, b.zeroOrMore(borKeyword, bitwiseXorExpression))
      .skipIfOneChild();

    b.rule(logicalAndExpression).is(bitwiseOrExpression, b.zeroOrMore(andKeyword, bitwiseOrExpression))
      .skipIfOneChild();

    b.rule(logicalOrExpression).is(logicalAndExpression, b.zeroOrMore(orKeyword, logicalAndExpression))
      .skipIfOneChild();

    b.rule(logicalXorExpression).is(logicalOrExpression, b.zeroOrMore(xorKeyword, logicalOrExpression))
      .skipIfOneChild();

    b.rule(shortCircuitOrElseExpression).is(logicalXorExpression, b.zeroOrMore(orelseKeyword, logicalXorExpression))
      .skipIfOneChild();

    b.rule(shortCircuitAndAlsoExpression).is(shortCircuitOrElseExpression,
      b.zeroOrMore(andalsoKeyword, shortCircuitOrElseExpression)).skipIfOneChild();

    b.rule(listOperationExpression).is(shortCircuitAndAlsoExpression,
      b.zeroOrMore(b.firstOf(plusplus, minusminus), shortCircuitAndAlsoExpression)).skipIfOneChild();

    b.rule(assignmentExpression).is(listOperationExpression, b.optional(matchop, expression)).skipIfOneChild();

    b.rule(funExpression).is(funKeyword, b.firstOf(b.sequence(b.optional(memberExpression, colon), funcArity),
      b.sequence(functionDeclarationsNoName, endKeyword)), b.optional(arguments));
    b.rule(functionDeclarationsNoName).is(functionDeclarationNoName, b.zeroOrMore(semi,
      functionDeclarationNoName));
    b.rule(functionDeclarationNoName).is(arguments, b.optional(guardSequenceStart), arrow, statements);

    b.rule(sendExpression).is(assignmentExpression, b.optional(exclamation, assignmentExpression)).skipIfOneChild();

    b.rule(caseExpression).is(caseKeyword, expression, ofKeyword, patternStatements, endKeyword);

    b.rule(ifExpression).is(ifKeyword, branchExps, endKeyword);

    b.rule(tryExpression).is(tryKeyword, statements, b.optional(ofKeyword, patternStatements), b.firstOf(b.sequence(catchExpression,
      afterExpression), catchExpression, afterExpression), endKeyword);

    b.rule(afterExpression).is(afterKeyword, statements);

    b.rule(catchExpression).is(catchKeyword, catchPatternStatements);

    b.rule(receiveExpression).is(receiveKeyword, b.firstOf(b.sequence(patternStatements, b.optional(afterKeyword, expression, arrow,
      statements)), b.sequence(afterKeyword, expression, arrow, statements)), endKeyword);

    b.rule(blockExpression).is(beginKeyword, statements, endKeyword);

    b.rule(expression).is(b.optional(catchKeyword), sendExpression);
  }

  /**
   * A.4 Statement
   */
  private static void statements(LexerlessGrammarBuilder b) {
    b.rule(expressionStatement).is(expression);
    b.rule(statement).is(expressionStatement);
    b.rule(statements).is(statement, b.zeroOrMore(comma, statement));

  }

  public static void branchAndGuardExpressions(LexerlessGrammarBuilder b) {
    b.rule(branchExps).is(branchExp, b.zeroOrMore(semi, branchExp));
    b.rule(branchExp).is(guardSequence, arrow, statements);

    b.rule(patternStatements).is(patternStatement, b.zeroOrMore(semi, patternStatement));
    b.rule(patternStatement).is(pattern, b.optional(guardSequenceStart), arrow, statements);

    b.rule(catchPatternStatements).is(catchPatternStatement, b.zeroOrMore(semi, catchPatternStatement));
    b.rule(catchPatternStatement).is(catchPattern, b.optional(guardSequenceStart), arrow, statements);
    b.rule(pattern).is(assignmentExpression);
    b.rule(catchPattern).is(b.optional(atomOrIdentifier, colon), expression, b.optional(colon, identifier));

    b.rule(guardSequenceStart).is(whenKeyword, guardSequence);

    b.rule(guardSequence).is(guard, b.zeroOrMore(semi, guard));
    b.rule(guard).is(guardExpression, b.zeroOrMore(comma, guardExpression));
    b.rule(guardExpression).is(expression);
  }

  private static Object punctuator(String value, LexerlessGrammarBuilder b) {
    return b.sequence(value, spacing);
  }

  private static Object punctuator(String value, Object element, LexerlessGrammarBuilder b) {
    return b.sequence(value, element, spacing);
  }

  private static Object keyword(String value, LexerlessGrammarBuilder b) {
    return b.sequence(value, b.nextNot(letterOrDigit), spacing);
  }

  private static Object semiKeyword(String value, LexerlessGrammarBuilder b) {
    return b.sequence(value, b.nextNot(letterOrDigit), spacing);
  }

}
