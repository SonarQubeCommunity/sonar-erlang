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
package org.sonar.plugins.erlang.core.cpd;

import net.sourceforge.pmd.cpd.SourceCode;
import net.sourceforge.pmd.cpd.TokenEntry;
import net.sourceforge.pmd.cpd.Tokens;
import org.junit.Test;
import org.sonar.plugins.erlang.cpd.ErlangTokenizer;

import java.io.File;
import java.nio.charset.Charset;

import static org.fest.assertions.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ErlangTokenizerTest {

  @Test
  public void test() {
    ErlangTokenizer tokenizer = new ErlangTokenizer(Charset.forName("UTF-8"));
    SourceCode source = mock(SourceCode.class);
    when(source.getFileName()).thenReturn(
        new File("src/test/resources/cpd/person.erl").getAbsolutePath());
    Tokens tokens = new Tokens();
    tokenizer.tokenize(source, tokens);
    assertThat(tokens.getTokens().size()).isGreaterThan(1);
    assertThat(tokens.getTokens().get(tokens.size() - 1)).isEqualTo(TokenEntry.getEOF());
  }

}
