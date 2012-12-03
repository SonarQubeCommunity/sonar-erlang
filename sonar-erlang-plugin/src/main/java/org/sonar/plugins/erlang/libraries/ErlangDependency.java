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
package org.sonar.plugins.erlang.libraries;

import org.sonar.api.resources.Library;

import java.util.regex.Pattern;

public class ErlangDependency {

    private static final Pattern depNamePattern = Pattern.compile("(^\\{)([A-Za-z_0-9]*?)(\\,.*)", Pattern.DOTALL
            + Pattern.MULTILINE);
    private static final Pattern depVersionInTagPattern = Pattern.compile("(.*tag.*?\\\")(.*?)(\\\".*)", Pattern.DOTALL
            + Pattern.MULTILINE);
    private static final Pattern depVersionInBranchPattern = Pattern.compile("(.*branch.*?\\\")(.*?)(\\\".*)",
            Pattern.DOTALL + Pattern.MULTILINE);


    String name;
    String version;
    String key;

    public ErlangDependency(String oneDependency){
        name = depNamePattern.matcher(oneDependency).replaceFirst("$2");
        version = depVersionInTagPattern.matcher(oneDependency).replaceFirst("$2");
        if (version.length() == oneDependency.length()) {
            version = depVersionInBranchPattern.matcher(oneDependency).replaceFirst("$2");
            if (version.length() == oneDependency.length()) {
                if(oneDependency.contains("HEAD")){
                    version = "HEAD";
                } else {
                    version = "UNKOWN";
                }
            }
        }
        String[] parts = oneDependency.split(",");
        key = parts[3].replaceFirst("(.*:)(.*?)(\\\")", "$2").replaceAll("[\\\\/]", ":")
                .replaceAll("\\.git", "");
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

    public Library getAsLibrary(){
        Library lib = new Library(getKey(), getVersion());
        lib.setName(getName());
        return lib;
    }

}
