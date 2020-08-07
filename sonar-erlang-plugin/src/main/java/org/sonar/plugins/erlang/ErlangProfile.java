/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.sonar.plugins.erlang;

import org.sonar.api.profiles.AnnotationProfileParser;
import org.sonar.api.profiles.ProfileDefinition;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.profiles.XMLProfileParser;
import org.sonar.api.rules.ActiveRule;
import org.sonar.api.utils.ValidationMessages;
import org.sonar.erlang.checks.CheckList;
import org.sonar.plugins.erlang.languages.ErlangLanguage;

import java.util.List;

public class ErlangProfile extends ProfileDefinition {

  public static final String PROFILE_NAME = "Sonar way";

  private final AnnotationProfileParser annotationProfileParser;
  private final XMLProfileParser xmlProfileParser;

  public ErlangProfile(AnnotationProfileParser annotationProfileParser,
                       XMLProfileParser xmlProfileParser) {
    this.annotationProfileParser = annotationProfileParser;
    this.xmlProfileParser = xmlProfileParser;
  }

  @Override
  public RulesProfile createProfile(ValidationMessages validation) {
    /*
     * TODO: is this the best way how we can merge two rule list?
     */
    RulesProfile ret = RulesProfile.create(PROFILE_NAME, ErlangLanguage.KEY);

    RulesProfile checks = annotationProfileParser.parse(CheckList.REPOSITORY_KEY,
      CheckList.REPOSITORY_NAME, ErlangLanguage.KEY, CheckList.getChecks(), validation);

    RulesProfile dialyzer = xmlProfileParser.parseResource(getClass().getClassLoader(),
      "org/sonar/plugins/erlang/profile-default.xml", validation);

    List<ActiveRule> rules = checks.getActiveRules();
    rules.addAll(dialyzer.getActiveRules());

    ret.setActiveRules(rules);

    return ret;
  }

}
