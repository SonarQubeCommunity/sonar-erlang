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
package org.sonar.plugins.erlang;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.AstNodeType;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.erlang.parser.ErlangGrammarImpl;

import java.io.File;
import java.util.List;

import static org.mockito.Mockito.*;

public class ErlangHighlighterTest {

  private SensorContextTester context;
  private final File testModuleBasedir = new File("src/test/resources/");
  ErlangHighlighter erlangHighlighter;

  @Before
  public void setUp() {
    context = SensorContextTester.create(testModuleBasedir.getAbsoluteFile());
    erlangHighlighter = new ErlangHighlighter(context);
  }

  @Test
  public void testInit() {
    erlangHighlighter.init();
    List<AstNodeType> nodesToVisit = erlangHighlighter.getAstNodeTypesToVisit();

    Assert.assertEquals(List.of(ErlangGrammarImpl.numericLiteral, ErlangGrammarImpl.stringLiteral), nodesToVisit);
  }

  @Test
  public void testVisitNode() {
    ErlangHighlighter mockedErlangHighlighter = mock(ErlangHighlighter.class);
    Token mockedToken = mock(Token.class);
    AstNode astNodeNumericLiteral = new AstNode(ErlangGrammarImpl.numericLiteral, "somename", mockedToken);
    AstNode astNodeStringLiteral = new AstNode(ErlangGrammarImpl.stringLiteral, "somestringliteral", mockedToken);

    mockedErlangHighlighter.init();
    mockedErlangHighlighter.visitNode(astNodeNumericLiteral);
    verify(mockedErlangHighlighter).visitNode(astNodeNumericLiteral);
    mockedErlangHighlighter.visitNode(astNodeStringLiteral);
    verify(mockedErlangHighlighter).visitNode(astNodeStringLiteral);
  }

  @Test
  public void testVisitToken() {
    Trivia mockTrivia = mock(Trivia.class);
    Token mockedToken = mock(Token.class);
    when(mockedToken.getTrivia()).thenReturn(List.of(mockTrivia));

    ErlangHighlighter mockedErlangHighlighter = mock(ErlangHighlighter.class);

    mockedErlangHighlighter.init();
    mockedErlangHighlighter.visitToken(mockedToken);
    verify(mockedErlangHighlighter).visitToken(mockedToken);
  }

}
