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
package org.sonar.erlang.ast;

import com.google.common.base.Preconditions;
import com.sonar.sslr.api.AstNode;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.api.SourcePackage;
import org.sonar.erlang.api.ErlangMetric;;

public class FileVisitor  extends ErlangAstVisitor {

  @Override
  public void visitFile(AstNode astNode) {
    SourceFile sourceFile = createSourceFile(peekParentPackage(), getContext().getFile().getName());
    sourceFile.setMeasure(ErlangMetric.FILES, 1);
    getContext().addSourceCode(sourceFile);
  }

  @Override
  public void leaveFile(AstNode astNode) {
    Preconditions.checkState(getContext().peekSourceCode().isType(SourceFile.class));
    getContext().popSourceCode();
  }

  private static SourceFile createSourceFile(SourcePackage parentPackage, String fileName) {
    StringBuilder key = new StringBuilder();
    if (parentPackage != null && !"".equals(parentPackage.getKey())) {
      key.append(parentPackage.getKey());
      key.append("/");
    }
    key.append(fileName);
    return new SourceFile(key.toString(), fileName);
  }

}
