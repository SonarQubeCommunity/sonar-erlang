package org.sonar.plugins.erlang;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Grammar;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.highlighting.NewHighlighting;
import org.sonar.api.batch.sensor.highlighting.TypeOfText;
import org.sonar.erlang.api.ErlangKeyword;
import org.sonar.erlang.parser.ErlangGrammarImpl;
import org.sonar.squidbridge.SquidAstVisitor;
import org.sonar.sslr.parser.LexerlessGrammar;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by tkende on 2017. 02. 26..
 */
public class ErlangHighlighter extends SquidAstVisitor<LexerlessGrammar> implements AstAndTokenVisitor {

    private final SensorContext context;
    private Set<Token> docStringTokens;
    private NewHighlighting newHighlighting;

    public ErlangHighlighter(SensorContext context) {
        this.context = context;
    }

    @Override
    public void visitToken(Token token) {
        if (token.getType().equals(ErlangGrammarImpl.numericLiteral)) {
            highlight(token, TypeOfText.CONSTANT);

        } else if (token.getType() instanceof ErlangKeyword) {
            highlight(token, TypeOfText.KEYWORD);

        } else if (token.getType().equals(ErlangGrammarImpl.stringLiteral) && !docStringTokens.contains(token)) {
            highlight(token, TypeOfText.STRING);
        }

        for (Trivia trivia : token.getTrivia()) {
            highlight(trivia.getToken(), TypeOfText.COMMENT);
        }
    }

    @Override
    public void visitFile(@Nullable AstNode astNode) {
        docStringTokens = new HashSet<>();
        newHighlighting = context.newHighlighting();
        InputFile inputFile = context.fileSystem().inputFile(context.fileSystem().predicates().is(getContext().getFile().getAbsoluteFile()));
        newHighlighting.onFile(inputFile);
    }

    @Override
    public void leaveFile(@Nullable AstNode ast){
        newHighlighting.save();
    }

    private void highlight(Token token, TypeOfText typeOfText) {
        TokenLocation tokenLocation = new TokenLocation(token);
        newHighlighting.highlight(tokenLocation.startLine(), tokenLocation.startLineOffset(), tokenLocation.endLine(), tokenLocation.endLineOffset(), typeOfText);
    }

    public static class TokenLocation {

        private final int startLine;
        private final int startLineOffset;
        private final int endLine;
        private final int endLineOffset;

        public TokenLocation(Token token) {
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
