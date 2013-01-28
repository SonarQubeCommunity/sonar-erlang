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
package org.sonar.erlang.checks;

import java.util.ArrayList;
import java.util.List;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.erlang.api.ErlangGrammar;

@Rule(key = "MultipleBlankLines", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
public class MultipleBlankLinesCheck extends SquidCheck<ErlangGrammar> {

    @RuleProperty(key = "maxBlankLinesInsideFunctions", defaultValue = "1")
    public int maxBlankLinesInsideFunctions = 1;

    @RuleProperty(key = "maxBlankLinesOutsideFunctions", defaultValue = "2")
    public int maxBlankLinesOutsideFunctions = 2;


    private List<Integer> checkedLines = new ArrayList<Integer>();

    @Override
    public void init() {
        subscribeTo(GenericTokenType.IDENTIFIER);
    }

    @Override
    public void visitNode(AstNode ast) {
        if (!ast.getToken().isGeneratedCode() && !checkedLines.contains(ast.getToken().getLine())) {
            Token previousToken = getPreviousToken(ast);
            if (previousToken != null) {
                int previousLine = previousToken.getLine();
                if (checkBlankLines(ast, previousLine)) {
                    getContext().createLineViolation(this,
                            "Too many blank lines found, the threshold is {0}.",
                            ast.getToken().getLine(), getMaxFor(ast));
                }
            }
            checkedLines.add(ast.getToken().getLine());
        }

    }

    private Token getPreviousToken(AstNode ast) {
        AstNode node = ast.previousAstNode();
        while (node != null && ast.getTokenLine() == node.getLastToken().getLine()) {
            node = node.previousAstNode();
        }
        if(node!=null){
            return node.getLastToken();
        } else {
            return null;
        }
    }

    private boolean compare(int line1, int line2, int comp) {
        return (line1 - line2 - 1 > comp);
    }

    private int getMaxFor(AstNode ast) {
        if (ast.findFirstParent(getContext().getGrammar().clauseBody) != null) {
            return maxBlankLinesInsideFunctions;
        } else {
            return maxBlankLinesOutsideFunctions;
        }
    }

    private boolean checkTrivias(int previousLine, Token token, int compTo) {
        int prevLine = previousLine;
        for (Trivia trivias : token.getTrivia()) {
            if (compare(trivias.getToken().getLine(), prevLine, compTo)) {
                return true;
            }
            prevLine = trivias.getToken().getLine();
        }
        return compare(token.getLine(), prevLine, compTo);
    }

    private boolean checkBlankLines(AstNode ast, int previousLine) {
        int compTo = getMaxFor(ast);

        boolean check = compare(ast.getToken().getLine(), previousLine, compTo);
        if (check) {
            Token tokenWithTrivias = (ast.getToken().hasTrivia()) ? ast.getToken() : ast
                    .previousAstNode().getToken();
            if (tokenWithTrivias.hasTrivia()) {
                return checkTrivias(previousLine, tokenWithTrivias, compTo);
            }
        }
        return check;
    }

}
