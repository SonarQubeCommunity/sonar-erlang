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
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.api.Token;

import javax.annotation.Nullable;

import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.cpd.NewCpdTokens;
import org.sonar.squidbridge.SquidAstVisitor;
import org.sonar.sslr.parser.LexerlessGrammar;

/**
 * Created by tkende on 2017. 02. 26.
 */
public class ErlangCpdVisitor extends SquidAstVisitor<LexerlessGrammar> implements AstAndTokenVisitor {

    private final SensorContext context;
    private NewCpdTokens newCpdTokens;

    ErlangCpdVisitor(SensorContext context) {
        this.context = context;
    }

    @Override
    public void visitToken(Token token) {
        if (token.getType() == GenericTokenType.EOF) {
            return;
        }
        ErlangHighlighter.TokenLocation tokenLocation = new ErlangHighlighter.TokenLocation(token);
        newCpdTokens.addToken(
                tokenLocation.startLine(),
                tokenLocation.startLineOffset(),
                tokenLocation.endLine(),
                tokenLocation.endLineOffset(),
                token.getValue()
        );
    }

    @Override
    public void visitFile(@Nullable AstNode astNode) {
        newCpdTokens = context.newCpdTokens();
        InputFile inputFile = context.fileSystem().inputFile(context.fileSystem().predicates().is(getContext().getFile().getAbsoluteFile()));
        if (inputFile != null) {
            newCpdTokens.onFile(inputFile);
        }
    }

    @Override
    public void leaveFile(@Nullable AstNode astNode) {
        newCpdTokens.save();
    }

}
