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

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.highlighting.NewHighlighting;
import org.sonar.api.batch.sensor.highlighting.TypeOfText;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.SquidAstVisitor;
import org.sonar.sslr.parser.LexerlessGrammar;

import javax.annotation.Nullable;

/**
 * Created by tkende on 2017. 02. 26.
 */
public class ErlangHighlighter extends SquidAstVisitor<LexerlessGrammar> implements AstAndTokenVisitor {

    private final SensorContext context;
    private NewHighlighting newHighlighting;

    ErlangHighlighter(SensorContext context) {
        this.context = context;
    }

    @Override
    public void init() {
        subscribeTo(ErlangGrammarImpl.numericLiteral);
        subscribeTo(ErlangGrammarImpl.stringLiteral);
    }

    @Override
    public void visitNode(AstNode astNode) {
        if (astNode.getType().equals(ErlangGrammarImpl.numericLiteral)) {
            highlight(astNode, TypeOfText.CONSTANT);
        } else if (astNode.getType().equals(ErlangGrammarImpl.stringLiteral)) {
            highlight(astNode, TypeOfText.STRING);
        }
    }

    @Override
    public void visitToken(Token token) {
        for (Trivia trivia : token.getTrivia()) {
            highlight(trivia.getToken(), TypeOfText.COMMENT);
        }
    }

    @Override
    public void visitFile(@Nullable AstNode astNode) {
        newHighlighting = context.newHighlighting();
        InputFile inputFile = context.fileSystem().inputFile(context.fileSystem().predicates().is(getContext().getFile().getAbsoluteFile()));
        if (inputFile != null) {
            newHighlighting.onFile(inputFile);
        }
    }

    @Override
    public void leaveFile(@Nullable AstNode ast){
        newHighlighting.save();
    }

    private void highlight(AstNode astNode, TypeOfText typeOfText) {
        TokenLocation firstLocation = new TokenLocation(astNode.getToken());
        TokenLocation lastLocation = new TokenLocation(astNode.getLastToken());
        newHighlighting.highlight(firstLocation.startLine(), firstLocation.startLineOffset(), lastLocation.endLine(), lastLocation.endLineOffset(), typeOfText);
    }

    private void highlight(Token token, TypeOfText typeOfText) {
        TokenLocation tokenLocation = new TokenLocation(token);
        newHighlighting.highlight(tokenLocation.startLine(), tokenLocation.startLineOffset(), tokenLocation.endLine(), tokenLocation.endLineOffset(), typeOfText);
    }

    static class TokenLocation {

        private final int startLine;
        private final int startLineOffset;
        private final int endLine;
        private final int endLineOffset;

        TokenLocation(Token token) {
            this.startLine = token.getLine();
            this.startLineOffset = token.getColumn();

            String value = token.getValue();
            String[] lines = value.split("\r\n|\n|\r", -1);

            if (lines.length > 1) {
                endLine = token.getLine() + lines.length - 1;
                endLineOffset = lines[lines.length - 1].length();

            } else {
                this.endLine = this.startLine;
                this.endLineOffset = this.startLineOffset + token.getValue().length();
            }
        }

        public int startLine() {
            return startLine;
        }

        public int startLineOffset() {
            return startLineOffset;
        }

        public int endLine() {
            return endLine;
        }

        public int endLineOffset() {
            return endLineOffset;
        }
    }
}
