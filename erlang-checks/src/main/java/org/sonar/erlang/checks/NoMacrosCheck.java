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

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.erlang.api.ErlangGrammar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Rule(key = "NoMacros", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.REPOSITORY_NAME, priority = Priority.MAJOR)
public class NoMacrosCheck extends SquidCheck<ErlangGrammar> {

    @RuleProperty(key = "skipDefineInFlowControl", defaultValue = "true",
        description = "Set it false if you want to check macros in flow controls.")
    private boolean skipDefineInFlowControl = true;

    @RuleProperty(key = "allowLiteralMacros", defaultValue = "true",
        description = "Set it to false if you want to have warnings on macros like: -define(TIMEOUT, 1000).")
    private boolean allowLiteralMacros = true;

    @RuleProperty(key = "ignoredMacroNames", defaultValue = "",
        description = "Comma separated list of ignored macro names.")
    private String ignoredMacroNames = "";

    private List<String> ignoreList = new ArrayList<String>();

    Function<String, String> trimItems = new Function<String, String>() {
        public String apply(String arg0) {
            return arg0.trim();
        };
    };

    private ErlangGrammar g;

    @Override
    public void init() {
        g = getContext().getGrammar();
        subscribeTo(g.defineAttr);
        ignoreList.addAll(Lists.transform(Arrays.asList(ignoredMacroNames.split(",")), trimItems));
    }

    @Override
    public void visitNode(AstNode astNode) {
        if (hasFlowControlParent(astNode) && isNotLiteralMacro(astNode) && isNotInIgnoreList(astNode)) {
            getContext().createLineViolation(this, "Do not use macros.", astNode.getTokenLine());
        }
    }

    private boolean isNotInIgnoreList(AstNode astNode) {
        return !ignoreList.contains(getMacroName(astNode));
    }

    private boolean isNotLiteralMacro(AstNode astNode) {
        return (astNode.hasChildren(g.funcDecl) && allowLiteralMacros) || (!allowLiteralMacros);
    }

    private boolean hasFlowControlParent(AstNode astNode) {
        return (!astNode.hasParents(g.flowControlAttr) || !skipDefineInFlowControl);
    }

    private String getMacroName(AstNode astNode) {
        // TODO: these kind of addressing are ugly... is there any better way?
        AstNode token = (astNode.findDirectChildren(GenericTokenType.IDENTIFIER).size() > 1) ? astNode.findDirectChildren(GenericTokenType.IDENTIFIER).get(1) : null;
        if (token == null) {
            token = astNode.findFirstDirectChild(g.funcDecl).findFirstDirectChild(GenericTokenType.IDENTIFIER);
        }
        return token.getTokenOriginalValue();
    }

    public void setSkipDefineInFlowControl(boolean skipDefineInFlowControl) {
        this.skipDefineInFlowControl = skipDefineInFlowControl;
    }

    public void setAllowLiteralMacros(boolean allowLiteralMacros) {
        this.allowLiteralMacros = allowLiteralMacros;
    }

    public void setIgnoredMacroNames(String ignoredMacroNames) {
        this.ignoredMacroNames = ignoredMacroNames;
    }

}
