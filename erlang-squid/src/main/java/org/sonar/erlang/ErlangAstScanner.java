/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.erlang;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.impl.Parser;

import org.sonar.squidbridge.AstScanner;

import org.sonar.squidbridge.SourceCodeBuilderCallback;

import org.sonar.squidbridge.SourceCodeBuilderVisitor;

import org.sonar.squidbridge.SquidAstVisitor;

import org.sonar.squidbridge.SquidAstVisitorContextImpl;

import org.sonar.squidbridge.metrics.CommentsVisitor;

import org.sonar.squidbridge.metrics.ComplexityVisitor;

import org.sonar.squidbridge.metrics.CounterVisitor;

import org.sonar.squidbridge.metrics.LinesOfCodeVisitor;

import org.sonar.squidbridge.metrics.LinesVisitor;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.metrics.BranchesOfRecursion;
import org.sonar.erlang.metrics.ErlangComplexityVisitor;
import org.sonar.erlang.metrics.ErlangStatementVisitor;
import org.sonar.erlang.metrics.NumberOfFunctionArgument;
import org.sonar.erlang.metrics.PublicDocumentedApiCounter;
import org.sonar.erlang.parser.ErlangGrammarImpl;

import org.sonar.squidbridge.api.SourceClass;

import org.sonar.squidbridge.api.SourceCode;

import org.sonar.squidbridge.api.SourceFunction;

import org.sonar.squidbridge.api.SourceProject;
import org.sonar.sslr.parser.LexerlessGrammar;
import org.sonar.sslr.parser.ParserAdapter;

import java.nio.charset.Charset;

public final class ErlangAstScanner {

  private ErlangAstScanner() {
  }

  @SafeVarargs
  public static AstScanner<LexerlessGrammar> create(Charset charset,
                                                    SquidAstVisitor<LexerlessGrammar>... visitors) {
    final SquidAstVisitorContextImpl<LexerlessGrammar> context = new SquidAstVisitorContextImpl<>(
      new SourceProject("Erlang Project"));
    final Parser<LexerlessGrammar> parser = new ParserAdapter<>(charset, ErlangGrammarImpl.createGrammar());

    AstScanner.Builder<LexerlessGrammar> builder = AstScanner.builder(context)
      .setBaseParser(parser);

    /* Metrics */
    builder.withMetrics(ErlangMetric.values());

    /* Comments */
    builder.setCommentAnalyser(new ErlangCommentAnalyser());

    /* Files */
    builder.setFilesMetric(ErlangMetric.FILES);

    /* Classes = modules */
    builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<>(
            (parentSourceCode, astNode) -> {
              String className = astNode.getFirstDescendant(ErlangGrammarImpl.moduleAttr).getFirstChild(ErlangGrammarImpl.atom).getTokenValue();
              SourceClass cls = new SourceClass(className + ":"
                + astNode.getToken().getLine());
              cls.setStartAtLine(astNode.getTokenLine());
              return cls;
            }, ErlangGrammarImpl.module));

    builder.withSquidAstVisitor(CounterVisitor.<LexerlessGrammar>builder().setMetricDef(
      ErlangMetric.MODULES).subscribeTo(ErlangGrammarImpl.module).build());

    /* Functions */
    builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<>(
      new SourceCodeBuilderCallback() {
        @Override
        public SourceCode createSourceCode(SourceCode parentSourceCode, AstNode astNode) {
          String functionKey = getFunctionKey(astNode);
          SourceFunction function = new SourceFunction(functionKey);
          function.setStartAtLine(astNode.getTokenLine());
          return function;
        }

        private String getFunctionKey(AstNode ast) {
          if (ast.getType().equals(ErlangGrammarImpl.funExpression)) {
            AstNode funcArity = ast.getFirstChild(ErlangGrammarImpl.funcArity);
            if (funcArity == null) {
              AstNode args = ast.getFirstDescendant(ErlangGrammarImpl.functionDeclarationNoName).getFirstChild(ErlangGrammarImpl.arguments);
              return "FUN/" + countArgs(args) + ":"
                + ast.getTokenLine() + "," + ast.getToken().getColumn();
            } else {
              return "FUN/" + funcArity.getTokenOriginalValue() + "/"
                + funcArity.getChildren(ErlangGrammarImpl.literal).get(1).getTokenOriginalValue();
            }
          } else {
            AstNode clause;
            boolean isDec = false;
            if (ast.getType().equals(ErlangGrammarImpl.functionDeclaration)) {
              clause = ast.getFirstDescendant(ErlangGrammarImpl.functionClause);
              isDec = true;
            } else {
              clause = ast;
            }
            String functionName = clause.getFirstChild(ErlangGrammarImpl.clauseHead)
              .getTokenValue();
            return functionName + "/" + getArity(clause) + ((!isDec) ? "c" : "") + ":"
              + clause.getTokenLine();
          }
        }

        private String getArity(AstNode ast) {
          AstNode args = ast.getFirstChild(ErlangGrammarImpl.clauseHead)
            .getFirstChild(ErlangGrammarImpl.funcDecl)
            .getFirstChild(ErlangGrammarImpl.arguments);
          return countArgs(args);
        }

        private String countArgs(AstNode args) {
          int num = args.getNumberOfChildren() > 3
            ? args.getChildren(ErlangGrammarImpl.comma).size() + 1
            : args.getNumberOfChildren() - 2;
          return String.valueOf(num);
        }
      }, ErlangGrammarImpl.functionDeclaration, ErlangGrammarImpl.functionClause, ErlangGrammarImpl.funExpression));

    builder.withSquidAstVisitor(CounterVisitor.<LexerlessGrammar>builder().setMetricDef(
      ErlangMetric.FUNCTIONS).subscribeTo(ErlangGrammarImpl.functionDeclaration).build());

    /* Metrics */

    builder.withSquidAstVisitor(new LinesVisitor<>(ErlangMetric.LINES));
    builder.withSquidAstVisitor(new LinesOfCodeVisitor<>(
      ErlangMetric.LINES_OF_CODE));

    builder.withSquidAstVisitor(CommentsVisitor.<LexerlessGrammar>builder().withCommentMetric(
      ErlangMetric.COMMENT_LINES)
      .withNoSonar(true)
      .withIgnoreHeaderComment(false).build());

    /* Statements */
    builder.withSquidAstVisitor(new ErlangStatementVisitor());

    /* Complexity */
    builder.withSquidAstVisitor(new ErlangComplexityVisitor());

    /* Public API counter */
    builder.withSquidAstVisitor(new PublicDocumentedApiCounter());

    /* Number of function arguments */
    builder.withSquidAstVisitor(new NumberOfFunctionArgument());

    /* Branches of recursion */
    builder.withSquidAstVisitor(new BranchesOfRecursion());

    /* Number of fun expressions */
    builder.withSquidAstVisitor(ComplexityVisitor.<LexerlessGrammar>builder().setMetricDef(
      ErlangMetric.NUM_OF_FUN_EXRP).subscribeTo(ErlangGrammarImpl.funExpression).build());

    /* Number of function clauses */
    builder.withSquidAstVisitor(ComplexityVisitor.<LexerlessGrammar>builder().setMetricDef(
      ErlangMetric.NUM_OF_FUN_CLAUSES).subscribeTo(ErlangGrammarImpl.functionClause).build());

    /* Number of macro definitions */
    builder.withSquidAstVisitor(ComplexityVisitor.<LexerlessGrammar>builder().setMetricDef(
      ErlangMetric.NUM_OF_MACROS).subscribeTo(ErlangGrammarImpl.defineAttr).build());

    /* External visitors (typically Check ones) */
    for (SquidAstVisitor<LexerlessGrammar> visitor : visitors) {
      builder.withSquidAstVisitor(visitor);
    }
    return builder.build();
  }
}
