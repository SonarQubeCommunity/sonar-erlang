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
package org.sonar.erlang.checks;

import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.api.ErlangPunctuator;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

@Rule(key = "NoEmacsStyleLeadingComma", priority = Priority.MAJOR,
  cardinality = Cardinality.SINGLE, name = "NoEmacsStyleLeadingComma",
  description = "No Emacs style leading comma")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NoEmacsStyleLeadingCommasCheck extends SquidCheck<ErlangGrammar> implements
    AstAndTokenVisitor {

  private Token previousToken;

  @Override
  public void visitFile(AstNode astNode) {
  }

  @Override
  public void leaveFile(AstNode astNode) {
  }

  public void visitToken(Token token) {
    if (previousToken == null || (previousToken.getLine() != token.getLine())) {
      if (token.getType().equals(ErlangPunctuator.COMMA)) {
        getContext().createLineViolation(this, "No Emacs-style leading commas.",
            token.getLine());
      }
      previousToken = token;
    }

  }

}
