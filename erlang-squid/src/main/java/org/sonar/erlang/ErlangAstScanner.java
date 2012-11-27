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

import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.api.ErlangPunctuator;
import org.sonar.erlang.metrics.BranchesOfRecursion;
import org.sonar.erlang.metrics.ErlangComplexityVisitor;
import org.sonar.erlang.metrics.ErlangStatementVisitor;
import org.sonar.erlang.metrics.NumberOfFunctionArgument;
import org.sonar.erlang.metrics.PublicDocumentedApiCounter;
import org.sonar.erlang.parser.ErlangParser;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.squid.AstScanner;
import com.sonar.sslr.squid.SourceCodeBuilderCallback;
import com.sonar.sslr.squid.SourceCodeBuilderVisitor;
import com.sonar.sslr.squid.SquidAstVisitor;
import com.sonar.sslr.squid.SquidAstVisitorContextImpl;
import com.sonar.sslr.squid.metrics.CommentsVisitor;
import com.sonar.sslr.squid.metrics.ComplexityVisitor;
import com.sonar.sslr.squid.metrics.CounterVisitor;
import com.sonar.sslr.squid.metrics.LinesOfCodeVisitor;
import com.sonar.sslr.squid.metrics.LinesVisitor;
import org.sonar.squid.api.SourceClass;
import org.sonar.squid.api.SourceCode;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.api.SourceFunction;
import org.sonar.squid.api.SourceProject;
import org.sonar.squid.indexer.QueryByType;

import java.io.File;
import java.nio.charset.Charset;
import java.util.Collection;

public final class ErlangAstScanner {

  private ErlangAstScanner() {
  }

  public static SourceFile scanSingleFile(File file, SquidAstVisitor<ErlangGrammar>... visitors) {
    if (!file.isFile()) {
      throw new IllegalArgumentException("File '" + file + "' not found.");
    }
    AstScanner<ErlangGrammar> scanner = create(
        new ErlangConfiguration(Charset.forName("UTF-8")), visitors);
    scanner.scanFile(file);
    Collection<SourceCode> sources = scanner.getIndex().search(
        new QueryByType(SourceFile.class));
    if (sources.size() != 1) {
      throw new IllegalStateException("Only one SourceFile was expected whereas "
        + sources.size() + " has been returned.");
    }
    return (SourceFile) sources.iterator().next();
  }

  public static AstScanner<ErlangGrammar> create(ErlangConfiguration conf,
      SquidAstVisitor<ErlangGrammar>... visitors) {
    final SquidAstVisitorContextImpl<ErlangGrammar> context = new SquidAstVisitorContextImpl<ErlangGrammar>(
        new SourceProject("Erlang Project"));
    final Parser<ErlangGrammar> parser = ErlangParser.create(conf);

    AstScanner.Builder<ErlangGrammar> builder = AstScanner.<ErlangGrammar> builder(context)
        .setBaseParser(parser);
    final ErlangGrammar grammar = parser.getGrammar();

    /* Metrics */
    builder.withMetrics(ErlangMetric.values());

    /* Comments */
    builder.setCommentAnalyser(new ErlangCommentAnalyser());

    /* Files */
    builder.setFilesMetric(ErlangMetric.FILES);

    /* Classes = modules */
    builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<ErlangGrammar>(
        new SourceCodeBuilderCallback() {
          public SourceCode createSourceCode(SourceCode parentSourceCode, AstNode astNode) {
            String className = astNode.getChild(3).getTokenValue();
            SourceClass cls = new SourceClass(className + ":"
              + astNode.getToken().getLine());
            cls.setStartAtLine(astNode.getTokenLine());
            return cls;
          }
        }, grammar.moduleAttr));

    builder.withSquidAstVisitor(CounterVisitor.<ErlangGrammar> builder().setMetricDef(
        ErlangMetric.MODULES).subscribeTo(grammar.moduleAttr).build());

    /* Functions */
    builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<ErlangGrammar>(
        new SourceCodeBuilderCallback() {
          public SourceCode createSourceCode(SourceCode parentSourceCode, AstNode astNode) {
            SourceFunction function = new SourceFunction(getFunctionKey(astNode));
            function.setStartAtLine(astNode.getTokenLine());
            return function;
          }

          private String getFunctionKey(AstNode ast) {
            AstNode clause = null;
            boolean isDec = false;
            if (ast.getType().equals(grammar.functionDeclaration)) {
              clause = ast.findFirstChild(grammar.functionClause);
              isDec = true;
            } else {
              clause = ast;
            }
            String functionName = clause.findFirstDirectChild(grammar.clauseHead)
                .getTokenValue();
            return functionName + "/" + ((!isDec) ? "c" : "") + getArity(clause) + ":"
              + clause.getTokenLine();
          }

          private String getArity(AstNode ast) {
            AstNode args = ast.findFirstDirectChild(grammar.clauseHead)
                .findFirstDirectChild(grammar.funcDecl).findFirstDirectChild(
                    grammar.arguments);
            int num = args.getNumberOfChildren() > 3 ? args.findDirectChildren(
                ErlangPunctuator.COMMA).size() + 1 : args.getNumberOfChildren() - 2;
            return String.valueOf(num);
          }
        }, grammar.functionDeclaration, grammar.functionClause));

    builder.withSquidAstVisitor(CounterVisitor.<ErlangGrammar> builder().setMetricDef(
        ErlangMetric.FUNCTIONS).subscribeTo(grammar.functionDeclaration).build());

    /* Metrics */

    builder.withSquidAstVisitor(new LinesVisitor<ErlangGrammar>(ErlangMetric.LINES));
    builder.withSquidAstVisitor(new LinesOfCodeVisitor<ErlangGrammar>(
        ErlangMetric.LINES_OF_CODE));

    builder.withSquidAstVisitor(CommentsVisitor.<ErlangGrammar> builder().withCommentMetric(
        ErlangMetric.COMMENT_LINES)
        .withBlankCommentMetric(ErlangMetric.COMMENT_BLANK_LINES).withNoSonar(true)
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
    builder.withSquidAstVisitor(ComplexityVisitor.<ErlangGrammar> builder().setMetricDef(
        ErlangMetric.NUM_OF_FUN_EXRP).subscribeTo(grammar.funExpression).build());

    /* Number of function clauses */
    builder.withSquidAstVisitor(ComplexityVisitor.<ErlangGrammar> builder().setMetricDef(
        ErlangMetric.NUM_OF_FUN_CLAUSES).subscribeTo(grammar.functionClause).build());

    /* Number of macro definitions */
    builder.withSquidAstVisitor(ComplexityVisitor.<ErlangGrammar> builder().setMetricDef(
        ErlangMetric.NUM_OF_MACROS).subscribeTo(grammar.defineAttr).build());

    /* External visitors (typically Check ones) */
    for (SquidAstVisitor<ErlangGrammar> visitor : visitors) {
      builder.withSquidAstVisitor(visitor);
    }

    return builder.build();
  }
}
