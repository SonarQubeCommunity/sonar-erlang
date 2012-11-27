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

import org.sonar.erlang.ErlangAstScanner;
import org.sonar.erlang.ErlangConfiguration;
import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangMetric;

import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableList;
import com.sonar.sslr.squid.AstScanner;
import org.junit.Test;
import org.sonar.squid.api.SourceClass;
import org.sonar.squid.api.SourceCode;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.api.SourceProject;
import org.sonar.squid.indexer.QueryByType;

import java.io.File;
import java.util.Set;

import static org.fest.assertions.Assertions.assertThat;

public class ErlangAstScannerTest {

  @Test
  public void files() {
    AstScanner<ErlangGrammar> scanner = ErlangAstScanner.create(new ErlangConfiguration(
        Charsets.UTF_8));
    scanner.scanFiles(ImmutableList.of(new File("src/test/resources/metrics/lines.erl"),
        new File("src/test/resources/metrics/lines_of_code.erl")));
    SourceProject project = (SourceProject) scanner.getIndex().search(
        new QueryByType(SourceProject.class)).iterator().next();
    assertThat(project.getInt(ErlangMetric.FILES)).isEqualTo(2);
  }

  @Test
  public void comments() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/functions.erl"));
    assertThat(file.getInt(ErlangMetric.COMMENT_BLANK_LINES)).isEqualTo(6);
    assertThat(file.getInt(ErlangMetric.COMMENT_LINES)).isEqualTo(5);
    assertThat(file.getNoSonarTagLines()).contains(38);
    assertThat(file.getNoSonarTagLines().size()).isEqualTo(1);
  }

  @Test
  public void modules() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/functions.erl"));
    AstScanner<ErlangGrammar> scanner = ErlangAstScanner.create(new ErlangConfiguration(
        Charsets.UTF_8));
    scanner.scanFiles(ImmutableList.of(new File("src/test/resources/metrics/functions.erl")));
    SourceClass module = (SourceClass) scanner.getIndex().search(
        new QueryByType(SourceClass.class)).iterator().next();
    assertThat(module.getKey()).isEqualTo("functions:1");
    assertThat(file.getInt(ErlangMetric.MODULES)).isEqualTo(1);
  }

  @Test
  public void lines() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/lines.erl"));
    assertThat(file.getInt(ErlangMetric.LINES)).isEqualTo(5);
  }

  @Test
  public void publicAPIs() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/functions.erl"));
    assertThat(file.getInt(ErlangMetric.PUBLIC_API)).isEqualTo(7);
    assertThat(file.getInt(ErlangMetric.PUBLIC_DOC_API)).isEqualTo(4);
    assertThat(file.getInt(ErlangMetric.PUBLIC_DOCUMENTED_API_DENSITY)).isEqualTo((4 / 7));
  }

  @Test
  public void lines_of_code() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/lines_of_code.erl"));
    assertThat(file.getInt(ErlangMetric.LINES_OF_CODE)).isEqualTo(3);
  }

  @Test
  public void lines_of_code2() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/lines_of_code2.erl"));
    assertThat(file.getInt(ErlangMetric.LINES_OF_CODE)).isEqualTo(14);
  }

  @Test
  public void lines2() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/lines_of_code2.erl"));
    assertThat(file.getInt(ErlangMetric.LINES)).isEqualTo(18);
  }

  @Test
  public void functions2() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/lines_of_code2.erl"));
    assertThat(file.getInt(ErlangMetric.FUNCTIONS)).isEqualTo(2);
  }

  @Test
  public void statements() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/statements.erl"));
    assertThat(file.getInt(ErlangMetric.STATEMENTS)).isEqualTo(20);
  }

  @Test
  public void functions() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/functions.erl"));
    assertThat(file.getInt(ErlangMetric.FUNCTIONS)).isEqualTo(7);
  }

  @Test
  public void complexity() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/complexity.erl"));
    assertThat(file.getInt(ErlangMetric.COMPLEXITY)).isEqualTo(10);
  }

  @Test
  public void numOfFunExpr() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/funexpressions.erl"));
    assertThat(file.getInt(ErlangMetric.NUM_OF_FUN_EXRP)).isEqualTo(4);
  }

  @Test
  public void numOfFunctionArguments() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/funargs.erl"));
    assertThat(file.getInt(ErlangMetric.NUM_OF_FUNC_ARGS)).isEqualTo(21);
    Set<SourceCode> children = file.getChildren();
    SourceCode[] childrenArray = children.toArray(new SourceCode[children.size()]);

    assertThat(childrenArray[0].getInt(ErlangMetric.NUM_OF_FUNC_ARGS)).isEqualTo(0);
    assertThat(childrenArray[1].getInt(ErlangMetric.NUM_OF_FUNC_ARGS)).isEqualTo(6);
    assertThat(childrenArray[2].getInt(ErlangMetric.NUM_OF_FUNC_ARGS)).isEqualTo(1);
    assertThat(childrenArray[3].getInt(ErlangMetric.NUM_OF_FUNC_ARGS)).isEqualTo(14);
    assertThat(
        childrenArray[3].getChildren().toArray(
            new SourceCode[childrenArray[3].getChildren().size()])[0]
            .getInt(ErlangMetric.NUM_OF_FUNC_ARGS)).isEqualTo(7);
    assertThat(
        childrenArray[3].getChildren().toArray(
            new SourceCode[childrenArray[3].getChildren().size()])[1]
            .getInt(ErlangMetric.NUM_OF_FUNC_ARGS)).isEqualTo(7);

  }

  @Test
  public void branchesOfRecursion() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/branchesofrecursion.erl"));
    assertThat(file.getInt(ErlangMetric.BRANCHES_OF_RECURSION)).isEqualTo(3);
  }

  @Test
  public void numOfFunctionClauses() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/funargs.erl"));
    assertThat(file.getInt(ErlangMetric.NUM_OF_FUN_CLAUSES)).isEqualTo(5);
  }

  @Test
  public void numOfMacros() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File(
        "src/test/resources/metrics/macros.erl"));
    assertThat(file.getInt(ErlangMetric.NUM_OF_MACROS)).isEqualTo(2);
  }

}
