/*
 * SonarQube Erlang Plugin
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
package org.sonar.plugins.erlang.libraries;

import com.sonar.sslr.api.AstNode;
import org.sonar.api.resources.Library;
import org.sonar.erlang.parser.ErlangGrammarImpl;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

public class ErlangDependency {

  String name;
  String version;
  String key;

  public ErlangDependency() {
    super();
  }

  public String getName() {
    return name;
  }

  public String getVersion() {
    return version;
  }

  public String getKey() {
    return key;
  }

  protected void setName(String name) {
    this.name = name;
  }

  protected void setVersion(String version) {
    this.version = version;
  }

  protected void setKey(String key) {
    this.key = key;
  }

  public Library getAsLibrary() {
    Library lib = new Library(getKey(), getVersion());
    lib.setName(getName());
    return lib;
  }

  public void parseVersionInfo(AstNode astNode) {
    List<AstNode> vcsElements = astNode.getFirstDescendant(ErlangGrammarImpl.tupleLiteral).getChildren(ErlangGrammarImpl.expression);
    String urlStr = removeQuotes(vcsElements.get(1).getTokenValue());
    try {
      URL url = new URL(urlStr);
      if (!"git".equals(vcsElements.get(0).getTokenValue())) {
        setKey(url.getHost() + ":" + getName());
      } else {
        setKey(getKeyFromUrl(url.getPath()));
      }

    } catch (MalformedURLException e) {
      // Replace possible protocol settings, like git:// or things, like: git@ from the beginning
      String[] gitUrl = urlStr.replaceAll("^[a-z]+?(@|://)", "").split(":");
      if (gitUrl.length == 1) {
        gitUrl = gitUrl[0].split("[/\\\\]", 2);
      }
      setKey(getKeyFromUrl(gitUrl[1]));
    }

    if (vcsElements.size() < 3) {
      setVersion("HEAD");
    } else {
      AstNode versionInfo = vcsElements.get(2);
      AstNode tuple = versionInfo.getFirstDescendant(ErlangGrammarImpl.tupleLiteral);
      if (tuple == null) {
        setVersion(removeQuotes(versionInfo.getTokenValue()));
      } else {
        setVersion(removeQuotes(tuple.getDescendants(ErlangGrammarImpl.expression).get(1).getTokenValue()));
      }
    }
  }

  private String removeQuotes(String str) {
    return str.replaceAll("\"", "");
  }

  private String getKeyFromUrl(String str) {
    return str.replace(".git", "").replaceAll("^[/\\\\]", "").replaceAll("[/\\\\]", ":");
  }

}
