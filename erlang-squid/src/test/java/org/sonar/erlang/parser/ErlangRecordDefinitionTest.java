/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.erlang.parser;

import com.google.common.base.Joiner;
import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ErlangRecordDefinitionTest {
  private LexerlessGrammar b = ErlangGrammarImpl.createGrammar();

  @Test
  public void recordDefinitions() {
    assertThat(b.rule(ErlangGrammarImpl.recordAttr))
      .matches("-record(tm, {log, pending, transactions, checkpoints}).")
      .matches(code(
        "-record(fallback_args, {opaque,", "scope = global,",
        "module = mnesia_monitor:get_env(backup_module),",
        "use_default_dir = true,", "mnesia_dir,", "fallback_bup,",
        "fallback_tmp,", "skip_tables = [],", "keep_tables = [],",
        "default_op = keep_tables", "})."))
      .matches(code(
        "-record(expand, {module=[],			%Module name",
        "parameters=undefined,		%Module parameters",
        "package=\"\",			%Module package", "exports=[],			%Exports",
        "imports=[],			%Imports", "mod_imports,			%Module Imports",
        "compile=[],			%Compile flags",
        "records=dict:new(),		%Record definitions",
        "attributes=[],			%Attributes", "defined=[],			%Defined functions",
        "vcount=0,			%Variable counter", "func=[],			%Current function",
        "arity=[],			%Arity for current function",
        "fcount=0,			%Local fun count",
        "fun_index=0,			%Global index for funs", "bitdefault,", "bittypes",
        "})."))
      .matches(code(
        "-record(client, {",
        "  state = wait :: wait | request | response | response_body,",
        "  opts = [] :: [any()],",
        "  socket = undefined :: undefined | inet:socket(),",
        "  transport = undefined :: module(),",
        "  timeout = 5000 :: timeout(), %% @todo Configurable.",
        "  buffer = <<>> :: binary(),",
        "  connection = keepalive :: keepalive | close,",
        "  version = 'HTTP/1.1' :: cowboy:http_version(),",
        "  response_body = undefined :: undefined | non_neg_integer()",
        " })."
      ))
      .matches("-record(state, {last::calendar:datetime(), tref::timer:tref()}).")
      .matches("-record(auth, {\ntoken :: string() | binary()\n}).")
      .matches(code("-record(map, {dict = dict:new()   :: dict(),",
        "subst = dict:new()  :: dict(),",
        "modified = []       :: [Key :: term()],",
        "modified_stack = [] :: [{[Key :: term()],reference()}],",
        "ref = undefined     :: reference() | undefined})."))
      .matches("-record(fun_var, {'fun' :: fun((_) -> erl_types:erl_type()), deps :: [dep()], origin :: integer()}).")
      .matches((code("-record(cat, {}).")));
    ;
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

}
