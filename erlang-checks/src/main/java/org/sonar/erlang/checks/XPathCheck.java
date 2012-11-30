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

import org.sonar.erlang.api.ErlangGrammar;

import com.sonar.sslr.squid.checks.AbstractXPathCheck;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

@Rule(key = "XPath", priority = Priority.MAJOR, cardinality = Cardinality.MULTIPLE, name = "XPath",
    description = "<p>This rule allows to define some homemade Erlang rules with help of an XPath expression.</p>" +
        "<p>Violations are created depending on the return value of the XPath expression. If the XPath expression returns:</p>" +
        "<ul>" +
        "  <li>a single or list of AST nodes, then a line violation with the given message is created for each node</li>" +
        "  <li>a boolean, then a file violation with the given message is created only if the boolean is true</li>" +
        "  <li>anything else, no violation is created</li>" +
        "</ul>" +
        "<p>Here is an example of an XPath expression to log a violation on each if expression : //ifExpression</p>")
public class XPathCheck extends AbstractXPathCheck<ErlangGrammar> {

    private static final String DEFAULT_XPATH_QUERY = "";
    private static final String DEFAULT_MESSAGE = "The XPath expression matches this piece of code";

    @RuleProperty(key = "xpathQuery", defaultValue = "" + DEFAULT_XPATH_QUERY)
    public String xpathQuery = DEFAULT_XPATH_QUERY;

    @RuleProperty(key = "message", defaultValue = "" + DEFAULT_MESSAGE)
    public String message = DEFAULT_MESSAGE;

    @Override
    public String getXPathQuery() {
        return xpathQuery;
    }

    @Override
    public String getMessage() {
        return message;
    }

}
