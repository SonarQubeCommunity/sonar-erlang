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
package org.sonar.erlang.parser;

import org.sonar.erlang.ErlangConfiguration;
import org.sonar.erlang.api.ErlangGrammar;
import org.sonar.erlang.parser.ErlangParser;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ExtendedStackTrace;
import com.sonar.sslr.impl.events.ExtendedStackTraceStream;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangParserStatementTest {
  ExtendedStackTrace listener = new ExtendedStackTrace();
  Parser<ErlangGrammar> p = ErlangParser
      .create(new ErlangConfiguration(Charsets.UTF_8), listener);

  ErlangGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.statements);
  }

  @Test
  public void statements() {
    assertThat(p).matches((code("1,", "A")));
    assertThat(p).matches((code("1+3,", "<<A>>")));
  }

  @Test
  public void ifStatements() {
    assertThat(p).matches((code("if A =:= B -> ok end")));
    assertThat(p)
        .matches(
            (code("if A =:= B -> ok; true -> io:format(\"assert error in module ~p on line ~p~n\") end")));
  }

  @Test
  public void funStatements() {

    assertThat(p).matches(
        (code("fun (Name) ->" + "Spec = agner:spec(Name),"
          + "Searchable = string:to_lower(\"hElO\")" + "end")));

    assertThat(p)
        .matches(
            (code("fun	(Name) ->", "Spec = agner:spec(Name),",
                "Searchable = string:to_lower(\"hElO\");",
                "(Name, 23) when Name>=2 ->", "Spec = agner:spec(Name),",
                "Searchable = string:to_lower(\"hElO\")", "end")));

    assertThat(p).matches(
        (code("fun (Name) ->" + "Spec = agner:spec(Name),"
          + "Searchable = string:to_lower(\"hElO\")" + "end()")));

    assertThat(p).matches((code("fun module:function/3")));

    assertThat(p).matches((code("fun M:F/Arity")));

    assertThat(p).matches((code("fun ?MODULE:passthrough_fun_to_sql/1")));

  }

  @Test
  public void caseStatements() {
    assertThat(p).matches(
        (code("case Signal of", "{signal, _What, _From, _To} ->", "true;",
            "{signal, _What, _To} ->", "true;", "_Else -> false", "end")));
  }

  @Test
  public void sendStatements() {
    assertThat(p).matches((code("Client ! {self(), data_sent}")));
    assertThat(p).matches((code("Client ! {self(), data_sent}, A")));
    assertThat(p).matches((code("B, Client ! {self(), data_sent}, A")));
  }

  @Test
  public void receiveStatements() {
    assertThat(p).matches(
        (code("receive", "onhook ->", "disconnect(),", "idle();", "{connect, B} ->",
            "B ! {busy, self()},", "wait_for_onhook()", "after", "60000 ->",
            "disconnect(),", "error()", "end")));

    assertThat(p).matches((code("receive after To -> ok end")));
  }

  @Test
  public void tryStatements() {
    assertThat(p).matches(
        (code("try Exprs of Pattern when GuardSeq -> Body after AfterBody end")));

    assertThat(p).matches(
        (code("try Exprs catch ExpressionPattern -> ExpressionBody after AfterBody end")));

    assertThat(p).matches((code("try Exprs after AfterBody end")));

    assertThat(p).matches(
        (code("try", "{ok,Bin} = file:read(F, 1024*1024),", "binary_to_term(Bin)", "after",
            "file:close(F)", "end")));

    assertThat(p).matches(
        (code("try Expr", "catch", "throw:Term -> Term;",
            "exit:Reason -> {'EXIT',Reason};",
            "error:Reason -> {'EXIT',{Reason,erlang:get_stacktrace()}}", "end")));
  }

  @Test
  public void blockStatements() {
    assertThat(p).matches((code("begin a, S=2 end")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

  @After
  public void log() {
    ExtendedStackTraceStream.print(listener, System.out);
  }
}
