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

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.xpath.api.AstNodeXPathQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.design.Dependency;
import org.sonar.api.resources.Library;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.ErlangPlugin;
import org.sonar.plugins.erlang.core.Erlang;

import java.io.File;
import java.util.List;

public class ErlangLibrarySensor implements Sensor {

  private final static Logger LOG = LoggerFactory.getLogger(ErlangLibrarySensor.class);
  private Erlang erlang;

  public ErlangLibrarySensor(Erlang erlang) {
    this.erlang = erlang;
  }

  public void analyse(Project project, SensorContext context) {
    analyzeRebarConfigFile(project, context, project.getFileSystem().getBasedir());
  }

  private void analyzeRebarConfigFile(Resource<?> projectResource, SensorContext context, File baseDir) {
    String rebarConfigUrl = erlang.getConfiguration().getString(ErlangPlugin.REBAR_CONFIG_FILENAME_KEY,
        ErlangPlugin.REBAR_DEFAULT_CONFIG_FILENAME);
    File rebarConfigFile = new File(baseDir, rebarConfigUrl);
    LOG.warn("Try get libraries from: " + rebarConfigFile.getAbsolutePath());

    if (rebarConfigFile.exists()) {
      AstNode config = RebarConfigParser.create().parse(rebarConfigFile);

      String depsDir = getDepsDir(config);

      List<Object> dependencies = AstNodeXPathQuery.create("//tupleLiteral[.//*/@*='deps']//listLiteral/primaryExpression").selectNodes(config);
      for (Object dependencyObj : dependencies) {
        AstNode dependency = (AstNode) dependencyObj;
        List<Object> dependencyElementObjs = AstNodeXPathQuery.create("./tupleLiteral/expression").selectNodes(dependency);
        ErlangDependency erlangDep = new ErlangDependency();
        erlangDep.setName(((AstNode)dependencyElementObjs.get(0)).getTokenValue());
        erlangDep.parseVersionInfo(((AstNode)dependencyElementObjs.get(2)));

        Library depLib = erlangDep.getAsLibrary();
        Resource<?> to = getResourceFromLibrary(context, depLib);
        saveDependency(projectResource, context, to);
        File depRebarConfig = new File(baseDir.getPath().concat(File.separator + depsDir)
            .concat(File.separator + erlangDep.getName()));
        analyzeRebarConfigFile(to, context, depRebarConfig);
      }
    }
    LOG.debug("Libraries added: " + context);
  }

  private Resource<?> getResourceFromLibrary(SensorContext context, Library depLib) {
    Resource<?> to = context.getResource(depLib);
    if (to == null) {
      context.index(depLib);
      to = context.getResource(depLib);
    }
    return to;
  }

  private void saveDependency(Resource<?> projectResource, SensorContext context, Resource<?> to) {
    Dependency dependency = new Dependency(projectResource, to);
    dependency.setUsage("compile");
    dependency.setWeight(1);
    context.saveDependency(dependency);
  }

  private String getDepsDir(AstNode config) {
    // find lib dir: {lib_dirs,["deps"]}. or deps_dir?
    String ret = "deps";
    Object depsSetting = AstNodeXPathQuery.create("//tupleLiteral[.//*/@*='deps_dir']").selectSingleNode(config);
    if(depsSetting!=null){
      ret = ((AstNode)AstNodeXPathQuery.create(".//stringLiteral").selectSingleNode((AstNode)depsSetting)).getTokenValue();
    }
    return ret;
  }

  public final boolean shouldExecuteOnProject(Project project) {
    return project.getLanguage().equals(erlang);
  }

}
