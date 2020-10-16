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

import com.google.common.collect.Lists;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonar.api.batch.fs.FilePredicates;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.TextRange;
import org.sonar.api.batch.measure.MetricFinder;
import org.sonar.api.batch.rule.CheckFactory;
import org.sonar.api.batch.rule.Checks;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.batch.sensor.issue.NewIssue;
import org.sonar.api.batch.sensor.issue.NewIssueLocation;
import org.sonar.api.batch.sensor.measure.NewMeasure;
import org.sonar.squidbridge.AstScanner;

import org.sonar.squidbridge.SquidAstVisitor;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.rule.RuleKey;
import org.sonar.erlang.ErlangAstScanner;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.checks.CheckList;
import org.sonar.plugins.erlang.languages.ErlangLanguage;

import org.sonar.squidbridge.api.CheckMessage;

import org.sonar.squidbridge.api.SourceCode;

import org.sonar.squidbridge.api.SourceFile;

import org.sonar.squidbridge.indexer.QueryByType;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

public class ErlangSquidSensor implements Sensor {

  private static final Logger LOG = Loggers.get(ErlangSquidSensor.class);
  private final Checks<Object> checks;
  private final MetricFinder metricFinder;

  public ErlangSquidSensor(CheckFactory checkFactory, MetricFinder metricFinder) {
    this.checks = checkFactory
            .create(CheckList.REPOSITORY_KEY)
            .addAnnotatedChecks((Iterable<?>) CheckList.getChecks());
    this.metricFinder = metricFinder;
  }

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor
            .onlyOnLanguage(ErlangLanguage.KEY)
            .name("Erlang EUnit Squid Sensor");
  }

  @Override
  public void execute(SensorContext context) {
    FileSystem fileSystem = context.fileSystem();
    List<SquidAstVisitor<LexerlessGrammar>> visitors = new ArrayList<SquidAstVisitor<LexerlessGrammar>>((Collection) checks.all());
    visitors.add(new ErlangHighlighter(context));
    visitors.add(new ErlangCpdVisitor(context));
    AstScanner scanner = ErlangAstScanner.create(fileSystem.encoding(), visitors.toArray(new SquidAstVisitor[visitors.size()]));

    FilePredicates p = fileSystem.predicates();
    Iterable<java.io.File> inputFiles = fileSystem.files(p.and(p.hasType(InputFile.Type.MAIN), p.hasLanguage(ErlangLanguage.KEY)));
    scanner.scanFiles(Lists.newArrayList(inputFiles));

    save(context, scanner.getIndex().search(new QueryByType(SourceFile.class)));
  }

  private void save(SensorContext context, Collection<SourceCode> squidSourceFiles) {
    FileSystem fileSystem = context.fileSystem();
    for (SourceCode squidSourceFile : squidSourceFiles) {
      SourceFile squidFile = (SourceFile) squidSourceFile;

      InputFile inputFile = fileSystem.inputFile(fileSystem.predicates().hasAbsolutePath(squidFile.getKey()));

      if (inputFile != null) {
        saveMeasures(context, inputFile, squidFile);
        saveViolations(context, inputFile, squidFile);
      } else {
        LOG.warn("Cannot save analysis information for file {}. Unable to retrieve the associated sonar resource.", squidFile.getKey());
      }
    }
  }

  private void saveMeasures(SensorContext context, InputFile sonarFile, SourceFile squidFile) {
    NewMeasure<Serializable> m = context.newMeasure();
    m.forMetric(Objects.requireNonNull(metricFinder.findByKey(CoreMetrics.NCLOC_KEY)))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.LINES_OF_CODE))
            .save();

    m = context.newMeasure();
    m.forMetric(Objects.requireNonNull(metricFinder.findByKey(CoreMetrics.FUNCTIONS_KEY)))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.FUNCTIONS))
            .save();

    m = context.newMeasure();
    m.forMetric(Objects.requireNonNull(metricFinder.findByKey(CoreMetrics.STATEMENTS_KEY)))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.STATEMENTS))
            .save();

    m = context.newMeasure();
    m.forMetric(Objects.requireNonNull(metricFinder.findByKey(CoreMetrics.COMPLEXITY_KEY)))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.COMPLEXITY))
            .save();

    m = context.newMeasure();
    m.forMetric(Objects.requireNonNull(metricFinder.findByKey(CoreMetrics.COMMENT_LINES_KEY)))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.COMMENT_LINES))
            .save();

  }

  private void saveViolations(SensorContext context, InputFile sonarFile, SourceFile squidFile) {
    Collection<CheckMessage> messages = squidFile.getCheckMessages();
    if (messages != null) {
      for (CheckMessage message : messages) {
        RuleKey ruleKey = checks.ruleKey(message.getCheck());
        TextRange range = sonarFile.selectLine(message.getLine());
        assert ruleKey != null;
        NewIssue issue = context
                .newIssue()
                .forRule(ruleKey);

        NewIssueLocation location = issue.newLocation()
                .on(sonarFile)
                .at(range)
                .message(message.formatDefaultMessage());

        issue.at(location);
        issue.save();
      }
    }
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }
}
