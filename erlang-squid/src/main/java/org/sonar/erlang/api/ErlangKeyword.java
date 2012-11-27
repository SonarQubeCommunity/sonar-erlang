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
package org.sonar.erlang.api;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.TokenType;

public enum ErlangKeyword implements TokenType {
  AFTER("after"), AND("and"), ANDALSO("andalso"), BAND("band"), BEGIN("begin"), BNOT("bnot"), BOR(
      "bor"), BSL("bsl"), BSR("bsr"), BXOR("bxor"), CASE("case"), CATCH("catch"), COND("cond"), DIV(
      "div"), END("end"), FUN("fun"), IF("if"), LET("let"), NOT("not"), OF("of"), OR("or"), ORELSE(
      "orelse"), QUERY("query"), RECEIVE("receive"), REM("rem"), TRY("try"), WHEN("when"), XOR(
      "xor");

  private final String value;

  private ErlangKeyword(String value) {
    this.value = value;
  }

  public String getName() {
    return name();
  }

  public String getValue() {
    return value;
  }

  public boolean hasToBeSkippedFromAst(AstNode node) {
    return false;
  }

  public static String[] keywordValues() {
    ErlangKeyword[] keywordsEnum = ErlangKeyword.values();
    String[] keywords = new String[keywordsEnum.length];
    for (int i = 0; i < keywords.length; i++) {
      keywords[i] = keywordsEnum[i].getValue();
    }
    return keywords;
  }

}
