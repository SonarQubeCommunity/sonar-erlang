package org.sonar.plugins.erlang;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;

import javax.annotation.Nullable;

import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.cpd.NewCpdTokens;
import org.sonar.squidbridge.SquidAstVisitor;
import org.sonar.sslr.parser.LexerlessGrammar;

/**
 * Created by tkende on 2017. 02. 26..
 */
public class ErlangCpdVisitor extends SquidAstVisitor<LexerlessGrammar> implements AstAndTokenVisitor {

    private final SensorContext context;
    private NewCpdTokens newCpdTokens;

    public ErlangCpdVisitor(SensorContext context) {
        this.context = context;
    }

    @Override
    public void visitToken(Token token) {
        ErlangHighlighter.TokenLocation tokenLocation = new ErlangHighlighter.TokenLocation(token);
        newCpdTokens.addToken(
                tokenLocation.startLine(),
                tokenLocation.startLineOffset(),
                tokenLocation.endLine(),
                tokenLocation.endLineOffset(),
                ""
        );
    }

    @Override
    public void visitFile(@Nullable AstNode astNode) {
        newCpdTokens = context.newCpdTokens();
        InputFile inputFile = context.fileSystem().inputFile(context.fileSystem().predicates().is(getContext().getFile().getAbsoluteFile()));
        newCpdTokens.onFile(inputFile);
    }

    @Override
    public void leaveFile(@Nullable AstNode astNode) {
        newCpdTokens.save();
    }

}
