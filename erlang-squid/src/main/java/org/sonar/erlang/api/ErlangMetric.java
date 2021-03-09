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
package org.sonar.erlang.api;


import org.sonar.squidbridge.measures.CalculatedMetricFormula;

import org.sonar.squidbridge.measures.MetricDef;

public enum ErlangMetric implements MetricDef {
  FILES, LINES, LINES_OF_CODE, COMMENT_LINES, COMMENT_BLANK_LINES, STATEMENTS, COMPLEXITY, FUNCTIONS, MODULES, PUBLIC_API, PUBLIC_DOC_API, PUBLIC_DOCUMENTED_API_DENSITY,

  // Erlang specific metrics
  NUM_OF_FUN_EXRP, NUM_OF_FUN_CLAUSES, NUM_OF_MACROS, NUM_OF_RECORDS, INCLUDED_FILES, IMPORTED_MODULES, NUM_OF_FUNC_ARGS, DEPTH_OF_CASES, BRANCHES_OF_RECURSION;

  @Override
  public boolean aggregateIfThereIsAlreadyAValue() {
    return true;
  }

  @Override
  public CalculatedMetricFormula getCalculatedMetricFormula() {
    return null;
  }

  @Override
  public String getName() {
    return name();
  }

  @Override
  public boolean isCalculatedMetric() {
    return false;
  }

  @Override
  public boolean isThereAggregationFormula() {
    return true;
  }

}
