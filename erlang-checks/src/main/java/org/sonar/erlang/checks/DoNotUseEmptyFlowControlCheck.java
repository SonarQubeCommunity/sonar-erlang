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
package org.sonar.erlang.checks;

import com.google.common.base.Predicates;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;

import java.util.List;

import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.annotations.SqaleConstantRemediation;
import org.sonar.squidbridge.checks.SquidCheck;
import org.sonar.sslr.parser.LexerlessGrammar;

@Rule(key = "DoNotUseEmptyFlowControl", priority = Priority.MINOR)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
@SqaleConstantRemediation("5min")
public class DoNotUseEmptyFlowControlCheck extends SquidCheck<LexerlessGrammar> {

  List<ErlangGrammarImpl> flowControls = ImmutableList.of(
    ErlangGrammarImpl.ifdefAttr,
    ErlangGrammarImpl.ifndefAttr,
    ErlangGrammarImpl.elseAttr,
    ErlangGrammarImpl.endifAttr
  );

  @Override
  public void init() {
    subscribeTo(ImmutableList.copyOf(Collections2.filter(flowControls,
      Predicates.not(Predicates.equalTo(ErlangGrammarImpl.endifAttr)))
    ).toArray(new ErlangGrammarImpl[flowControls.size() - 1]));

  }

  @Override
  public void visitNode(AstNode node) {
    if (flowControls.contains(node.getNextSibling().getType())) {
      getContext().createLineViolation(this, "Do not use empty flow control.", node);
    }

  }

}
