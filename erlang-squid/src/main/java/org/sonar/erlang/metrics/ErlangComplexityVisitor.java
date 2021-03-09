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
package org.sonar.erlang.metrics;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.AstNodeType;

import org.sonar.squidbridge.SquidAstVisitor;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.sslr.parser.LexerlessGrammar;

/**
 * Erlang complexity calculation +1 for every function clause over 1 +1 for
 * every fun expression +1 for every branch expression (if) +1 for every pattern
 * statement (case, try) +1 for every catch pattern statement (catch)
 *
 * @author Tamas Kende
 */
public class ErlangComplexityVisitor extends SquidAstVisitor<LexerlessGrammar> {

  private AstNodeType[] complexityAstNodeType;

  @Override
  public void init() {
    complexityAstNodeType = new AstNodeType[]{
      // Entry points
      ErlangGrammarImpl.functionClause,

      // Branching nodes
      ErlangGrammarImpl.branchExp, ErlangGrammarImpl.patternStatement, ErlangGrammarImpl.catchPatternStatement,

      // TODO: Expressions? when increase? if there are more than one guard
      // and more than
      // one guard expression?
      // grammar.guardSequence
    };
    subscribeTo(complexityAstNodeType);
  }

  @Override
  public void visitNode(AstNode astNode) {
    increaseComplexity(1);
  }

  private void increaseComplexity(int i) {
    getContext().peekSourceCode().add(ErlangMetric.COMPLEXITY, i);
  }
}
