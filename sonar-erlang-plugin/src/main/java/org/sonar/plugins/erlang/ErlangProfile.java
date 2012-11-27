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
package org.sonar.plugins.erlang;

import org.sonar.erlang.checks.CheckList;

import org.sonar.api.profiles.AnnotationProfileParser;
import org.sonar.api.profiles.ProfileDefinition;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.profiles.XMLProfileParser;
import org.sonar.api.rules.ActiveRule;
import org.sonar.api.utils.ValidationMessages;
import org.sonar.plugins.erlang.core.Erlang;

import java.util.List;

public class ErlangProfile extends ProfileDefinition {

  public static final String REPOSITORY_KEY = "erlang_default";
  public static final String PROFILE_NAME = RulesProfile.SONAR_WAY_NAME;
  private final AnnotationProfileParser annotationProfileParser;
  private final XMLProfileParser xmlProfileParser;

  public ErlangProfile(AnnotationProfileParser annotationProfileParser,
      XMLProfileParser xmlProfileParser) {
    this.annotationProfileParser = annotationProfileParser;
    this.xmlProfileParser = xmlProfileParser;
  }

  @Override
  public RulesProfile createProfile(ValidationMessages validation) {
    RulesProfile ret = RulesProfile.create(REPOSITORY_KEY, Erlang.KEY);
    ret.setName(PROFILE_NAME);
    RulesProfile checks = annotationProfileParser.parse(CheckList.REPOSITORY_KEY,
        CheckList.SONAR_WAY_PROFILE, Erlang.KEY, CheckList.getChecks(), validation);
    RulesProfile dialyzer = xmlProfileParser.parseResource(getClass().getClassLoader(),
        "org/sonar/plugins/erlang/profile-default.xml", validation);

    List<ActiveRule> rules = checks.getActiveRules();
    rules.addAll(dialyzer.getActiveRules());

    ret.setActiveRules(rules);

    return ret;
  }

}
