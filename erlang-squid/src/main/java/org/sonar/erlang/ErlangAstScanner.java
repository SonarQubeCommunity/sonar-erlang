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
package org.sonar.erlang;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.squid.SourceCodeBuilderCallback;
import com.sonar.sslr.squid.SourceCodeBuilderVisitor;
import com.sonar.sslr.squid.SquidAstVisitor;
import com.sonar.sslr.squid.metrics.CommentsVisitor;
import com.sonar.sslr.squid.metrics.ComplexityVisitor;
import com.sonar.sslr.squid.metrics.CounterVisitor;
import com.sonar.sslr.squid.metrics.LinesOfCodeVisitor;
import com.sonar.sslr.squid.metrics.LinesVisitor;
import org.sonar.api.resources.InputFile;
import org.sonar.api.resources.InputFileUtils;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.ast.AstScanner;
import org.sonar.erlang.ast.FileVisitor;
import org.sonar.erlang.metrics.BranchesOfRecursion;
import org.sonar.erlang.metrics.ErlangComplexityVisitor;
import org.sonar.erlang.metrics.ErlangStatementVisitor;
import org.sonar.erlang.metrics.NumberOfFunctionArgument;
import org.sonar.erlang.metrics.PublicDocumentedApiCounter;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.erlang.parser.ErlangParser;
import org.sonar.squid.api.SourceClass;
import org.sonar.squid.api.SourceCode;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.api.SourceFunction;
import org.sonar.squid.indexer.QueryByType;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.io.File;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;

public final class ErlangAstScanner {

  private ErlangAstScanner() {
  }

  public static SourceFile scanSingleFile(File file, SquidAstVisitor<LexerlessGrammar>... visitors) {
    if (!file.isFile()) {
      throw new IllegalArgumentException("File '" + file + "' not found.");
    }
    AstScanner scanner = create(new ErlangConfiguration(Charset.forName("UTF-8")), visitors);
    InputFile inputFile = InputFileUtils.create(file.getParentFile(), file);
    scanner.scan(Collections.singleton(inputFile));
    Collection<SourceCode> sources = scanner.getIndex().search(new QueryByType(SourceFile.class));
    if (sources.size() != 1) {
      throw new IllegalStateException("Only one SourceFile was expected whereas " + sources.size() + " has been returned.");
    }
    return (SourceFile) sources.iterator().next();
  }

  public static AstScanner create(ErlangConfiguration conf,
      SquidAstVisitor<LexerlessGrammar>... visitors) {
    final Parser<LexerlessGrammar> parser = ErlangParser.create(conf);

    AstScanner builder = new AstScanner(parser);

    // /* Metrics */
    // builder.withMetrics(ErlangMetric.values());

    /* Comments */
    builder.setCommentAnalyser(new ErlangCommentAnalyser());

    /* Files */
    builder.withSquidAstVisitor(new FileVisitor());

    /* Classes = modules */

    builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<LexerlessGrammar>(
        new SourceCodeBuilderCallback() {
          public SourceCode createSourceCode(SourceCode parentSourceCode, AstNode astNode) {
            String className = astNode.getFirstDescendant(ErlangGrammarImpl.moduleAttr).getChild(3).getTokenValue();
            SourceClass cls = new SourceClass(className + ":"
              + astNode.getToken().getLine());
            cls.setStartAtLine(astNode.getTokenLine());
            return cls;
          }
        }, ErlangGrammarImpl.module));

    builder.withSquidAstVisitor(CounterVisitor.<LexerlessGrammar> builder().setMetricDef(
        ErlangMetric.MODULES).subscribeTo(ErlangGrammarImpl.module).build());

    /* Functions */
    builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<LexerlessGrammar>(
        new SourceCodeBuilderCallback() {
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
              AstNode clause = null;
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
                .getFirstChild(ErlangGrammarImpl.funcDecl).getFirstChild(
                    ErlangGrammarImpl.arguments);
            return countArgs(args);
          }

          private String countArgs(AstNode args) {
            int num = args.getNumberOfChildren() > 3 ? args.getChildren(
                ErlangGrammarImpl.comma).size() + 1 : args.getNumberOfChildren() - 2;
            return String.valueOf(num);
          }
        }, ErlangGrammarImpl.functionDeclaration, ErlangGrammarImpl.functionClause, ErlangGrammarImpl.funExpression));

    builder.withSquidAstVisitor(CounterVisitor.<LexerlessGrammar> builder().setMetricDef(
        ErlangMetric.FUNCTIONS).subscribeTo(ErlangGrammarImpl.functionDeclaration).build());

    /* Metrics */

    builder.withSquidAstVisitor(new LinesVisitor<LexerlessGrammar>(ErlangMetric.LINES));
    builder.withSquidAstVisitor(new LinesOfCodeVisitor<LexerlessGrammar>(
        ErlangMetric.LINES_OF_CODE));

    builder.withSquidAstVisitor(CommentsVisitor.<LexerlessGrammar> builder()
        .withCommentMetric(ErlangMetric.COMMENT_LINES)
        .withBlankCommentMetric(ErlangMetric.COMMENT_BLANK_LINES)
        .withNoSonar(true)
        .withIgnoreHeaderComment(false)
        .build());

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
    builder.withSquidAstVisitor(ComplexityVisitor.<LexerlessGrammar> builder().setMetricDef(
        ErlangMetric.NUM_OF_FUN_EXRP).subscribeTo(ErlangGrammarImpl.funExpression).build());

    /* Number of function clauses */
    builder.withSquidAstVisitor(ComplexityVisitor.<LexerlessGrammar> builder().setMetricDef(
        ErlangMetric.NUM_OF_FUN_CLAUSES).subscribeTo(ErlangGrammarImpl.functionClause).build());

    /* Number of macro definitions */
    builder.withSquidAstVisitor(ComplexityVisitor.<LexerlessGrammar> builder().setMetricDef(
        ErlangMetric.NUM_OF_MACROS).subscribeTo(ErlangGrammarImpl.defineAttr).build());

    /* External visitors (typically Check ones) */
    for (SquidAstVisitor<LexerlessGrammar> visitor : visitors) {
      builder.withSquidAstVisitor(visitor);
    }
    return builder;
  }
}
