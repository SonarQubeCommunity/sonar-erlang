/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
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

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;

import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.util.List;

public class NumberOfFunctionArgument extends SquidCheck<LexerlessGrammar> {

  List<ErlangGrammarImpl> nonArg = ImmutableList.of(ErlangGrammarImpl.lparenthesis,
    ErlangGrammarImpl.rparenthesis, ErlangGrammarImpl.comma);

  @Override
  public void init() {

    subscribeTo(ErlangGrammarImpl.clauseHead);

  }

  @Override
  public void visitNode(AstNode ast) {
    AstNode args = ast.getFirstChild(ErlangGrammarImpl.funcDecl).getFirstChild(
      ErlangGrammarImpl.arguments);
    int numOfArgs = 0;
    for (AstNode arg : args.getChildren()) {
      if (!nonArg.contains(arg.getType())) {
        numOfArgs++;
      }
    }
    getContext().peekSourceCode().add(ErlangMetric.NUM_OF_FUNC_ARGS, numOfArgs);

  }

}
