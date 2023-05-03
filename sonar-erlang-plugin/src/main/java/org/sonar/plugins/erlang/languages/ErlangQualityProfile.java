/*
 * SonarQube Erlang Plugin
 * Copyright © 2012-2018 Tamas Kende <kende.tamas@gmail.com>
 * Copyright © 2018 Denes Hegedus (Cursor Insight Ltd.) <hegedenes@cursorinsight.com>
 * Copyright © 2020 Andris Raugulis <moo@arthepsy.eu>
 * Copyright © 2021 Daniils Petrovs <dpetrovs@evolution.com>
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
package org.sonar.plugins.erlang.languages;

import org.apache.commons.lang.StringUtils;
import org.sonar.api.rules.RuleAnnotationUtils;
import org.sonar.api.server.profile.BuiltInQualityProfilesDefinition;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.check.BelongsToProfile;
import org.sonar.erlang.checks.CheckList;
import org.sonar.plugins.erlang.dialyzer.DialyzerRuleDefinition;
import org.sonar.plugins.erlang.elvis.ElvisRuleDefinition;
import org.sonar.plugins.erlang.xref.XrefRuleDefinition;

public class ErlangQualityProfile implements BuiltInQualityProfilesDefinition {

  public static final String PROFILE_NAME = "Sonar way";

  @Override
  public void define(Context context) {
    NewBuiltInQualityProfile profile = context.createBuiltInQualityProfile(PROFILE_NAME, ErlangLanguage.KEY);
    profile.setDefault(true);

    // add Erlang Checks rules
    for (Class<?> clazz :CheckList.getChecks()) {
      BelongsToProfile belongsToProfile = clazz.getAnnotation(BelongsToProfile.class);
      if ((belongsToProfile != null) && StringUtils.equals(belongsToProfile.title(), CheckList.REPOSITORY_NAME)) {
        String ruleKey = RuleAnnotationUtils.getRuleKey(clazz);
        profile.activateRule(CheckList.REPOSITORY_KEY, ruleKey);
      }
    }

    RulesDefinition.Context rulesContext;
    RulesDefinition.Repository rulesRepository;

    // add Dialyzer rules
    rulesContext = new RulesDefinition.Context();
    new DialyzerRuleDefinition().define(rulesContext);
    rulesRepository = rulesContext.repository(DialyzerRuleDefinition.REPOSITORY_KEY);
    if (rulesRepository != null && rulesRepository.rules() != null) {
      for (RulesDefinition.Rule rule : rulesRepository.rules()) {
          profile.activateRule(rulesRepository.key(), rule.key());
      }
    }

    // add Elvis rules
    rulesContext = new RulesDefinition.Context();
    new ElvisRuleDefinition().define(rulesContext);
    rulesRepository = rulesContext.repository(ElvisRuleDefinition.REPOSITORY_KEY);
    if (rulesRepository != null && rulesRepository.rules() != null) {
      for (RulesDefinition.Rule rule : rulesRepository.rules()) {
        profile.activateRule(rulesRepository.key(), rule.key());
      }
    }

    // add Xref rules
    rulesContext = new RulesDefinition.Context();
    new XrefRuleDefinition().define(rulesContext);
    rulesRepository = rulesContext.repository(XrefRuleDefinition.REPOSITORY_KEY);
    if (rulesRepository != null && rulesRepository.rules() != null) {
      for (RulesDefinition.Rule rule : rulesRepository.rules()) {
        profile.activateRule(rulesRepository.key(), rule.key());
      }
    }

    profile.done();
  }

}
