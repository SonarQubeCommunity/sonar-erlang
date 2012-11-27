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

public class ErlangRecordDefinitionTest {
  ExtendedStackTrace listener = new ExtendedStackTrace();
  Parser<ErlangGrammar> p = ErlangParser
      .create(new ErlangConfiguration(Charsets.UTF_8), listener);

  ErlangGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.recordAttr);
  }

  @Test
  public void recordDefinitions() {
    assertThat(p).matches((code("-record(tm, {log, pending, transactions, checkpoints}).")));
    assertThat(p)
        .matches(
            (code("-record(fallback_args, {opaque,", "scope = global,",
                "module = mnesia_monitor:get_env(backup_module),",
                "use_default_dir = true,", "mnesia_dir,", "fallback_bup,",
                "fallback_tmp,", "skip_tables = [],", "keep_tables = [],",
                "default_op = keep_tables", "}).")));

    assertThat(p)
        .matches(
            (code("-record(expand, {module=[],			%Module name",
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
                "}).")));
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

  @After
  public void log() {
    ExtendedStackTraceStream.print(listener, System.out);
  }

}
