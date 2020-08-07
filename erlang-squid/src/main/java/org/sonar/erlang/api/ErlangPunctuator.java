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
package org.sonar.erlang.api;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.TokenType;

public enum ErlangPunctuator implements TokenType {
  ARROW("->"), ARROWBACK("<-"), DOUBLEARROWBACK("<="), LCURLYBRACE("{"), RCURLYBRACE("}"), LPARENTHESIS(
    "("), RPARENTHESIS(")"), LBRACKET("["), RBRACKET("]"), DOT("."), SEMI(";"), COMMA(","), COLON(
    ":"), MATCHOP("="), PLUS("+"), MINUS("-"), STAR("*"), DIV("/"), LT("<"), GT(">"), LE(
    "=<"), GE(">="), EQUAL("=="), NOTEQUAL("/="), EQUAL2("=:="), NOTEQUAL2("=/="), BINSTART(
    "<<"), BINEND(">>"), LISTCOMP("||"), PIPE("|"), DOLLAR("$"), APOSTROPHE("'"), PLUSPLUS(
    "++"), MINUSMINUS("--"), NUMBERSIGN("#"), EXCLAMATION("!"), QUESTIONMARK("?");

  private final String value;

  private ErlangPunctuator(String word) {
    this.value = word;
  }

  @Override
  public String getName() {
    return name();
  }

  @Override
  public String getValue() {
    return value;
  }

  @Override
  public boolean hasToBeSkippedFromAst(AstNode node) {
    return false;
  }

}
