/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2017 Tamas Kende
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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.plugins.erlang;

import com.google.common.collect.Lists;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import org.sonar.api.ce.measure.RangeDistributionBuilder;
import org.sonar.api.rule.RuleKey;
import org.sonar.erlang.ErlangAstScanner;
import org.sonar.erlang.api.ErlangMetric;
import org.sonar.erlang.checks.CheckList;
import org.sonar.plugins.erlang.core.Erlang;

import org.sonar.squidbridge.api.CheckMessage;

import org.sonar.squidbridge.api.SourceCode;

import org.sonar.squidbridge.api.SourceFile;

import org.sonar.squidbridge.api.SourceFunction;

import org.sonar.squidbridge.indexer.QueryByParent;

import org.sonar.squidbridge.indexer.QueryByType;
import org.sonar.sslr.parser.LexerlessGrammar;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ErlangSquidSensor implements Sensor {

  private static final Logger LOG = LoggerFactory.getLogger(ErlangSquidSensor.class);

  private static final Number[] FUNCTIONS_DISTRIB_BOTTOM_LIMITS = {1, 2, 4, 6, 8, 10, 12, 20, 30};
  private static final Number[] FILES_DISTRIB_BOTTOM_LIMITS = {0, 5, 10, 20, 30, 60, 90};

  private final Checks<Object> checks;
  private final MetricFinder metricFinder;

  private AstScanner scanner;

  ErlangSquidSensor(CheckFactory checkFactory, MetricFinder metricFinder) {
    this.checks = checkFactory
            .create(CheckList.REPOSITORY_KEY)
            .addAnnotatedChecks(CheckList.getChecks().iterator());
    this.metricFinder = metricFinder;
  }

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor
      .onlyOnLanguage(Erlang.KEY)
      .name("Erlang EUnit Squid Sensor");
  }

  @Override
  public void execute(SensorContext context) {
    FileSystem fileSystem = context.fileSystem();
    List<SquidAstVisitor<LexerlessGrammar>> visitors = new ArrayList<SquidAstVisitor<LexerlessGrammar>>((Collection) checks.all());
    visitors.add(new ErlangHighlighter(context));
    visitors.add(new ErlangCpdVisitor(context));
    this.scanner = ErlangAstScanner.create(fileSystem.encoding(), visitors.toArray(new SquidAstVisitor[visitors.size()]));

    FilePredicates p = fileSystem.predicates();
    Iterable<java.io.File> inputFiles = fileSystem.files(p.and(p.hasType(InputFile.Type.MAIN), p.hasLanguage(Erlang.KEY)));
    scanner.scanFiles(Lists.newArrayList(inputFiles));

    save(context, scanner.getIndex().search(new QueryByType(SourceFile.class)));
  }

  private void save(SensorContext context, Collection<SourceCode> squidSourceFiles) {
    FileSystem fileSystem = context.fileSystem();
    for (SourceCode squidSourceFile : squidSourceFiles) {
      SourceFile squidFile = (SourceFile) squidSourceFile;

      InputFile inputFile = fileSystem.inputFile(fileSystem.predicates().hasAbsolutePath(squidFile.getKey()));

      if (inputFile != null) {
        saveFilesComplexityDistribution(context, inputFile, squidFile);
        saveFunctionsComplexityDistribution(context, inputFile, squidFile);
        saveMeasures(context, inputFile, squidFile);
        saveViolations(context, inputFile, squidFile);
      } else {
        LOG.warn("Cannot save analysis information for file {}. Unable to retrieve the associated sonar resource.", squidFile.getKey());
      }
    }
  }

  private void saveMeasures(SensorContext context, InputFile sonarFile, SourceFile squidFile) {
    NewMeasure<Serializable> m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.FILES_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.FILES))
            .save();

    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.LINES_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.LINES))
            .save();

    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.NCLOC_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.LINES_OF_CODE))
            .save();

    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.FUNCTIONS_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.FUNCTIONS))
            .save();

    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.STATEMENTS_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.STATEMENTS))
            .save();

    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.COMPLEXITY_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.COMPLEXITY))
            .save();

    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.COMMENT_LINES_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.COMMENT_LINES))
            .save();

    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.PUBLIC_API_KEY))
            .on(sonarFile)
            .withValue(squidFile.getInt(ErlangMetric.PUBLIC_API))
            .save();

    int publicUndocApi = squidFile.getInt(ErlangMetric.PUBLIC_API) - squidFile.getInt(ErlangMetric.PUBLIC_DOC_API);
    m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.PUBLIC_UNDOCUMENTED_API_KEY))
            .on(sonarFile)
            .withValue(publicUndocApi)
            .save();
  }

  private void saveFunctionsComplexityDistribution(SensorContext context, InputFile sonarFile, SourceFile squidFile) {
    Collection<SourceCode> squidFunctionsInFile = scanner.getIndex().search(
      new QueryByParent(squidFile), new QueryByType(SourceFunction.class));
    RangeDistributionBuilder complexityDistribution = new RangeDistributionBuilder(FUNCTIONS_DISTRIB_BOTTOM_LIMITS);
    for (SourceCode squidFunction : squidFunctionsInFile) {
      complexityDistribution.add(squidFunction.getDouble(ErlangMetric.COMPLEXITY));
    }
    NewMeasure<Serializable> m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.FUNCTION_COMPLEXITY_DISTRIBUTION_KEY))
            .on(sonarFile)
            .withValue(complexityDistribution.build())
            .save();
  }

  private void saveFilesComplexityDistribution(SensorContext context, InputFile sonarFile, SourceFile squidFile) {
    RangeDistributionBuilder complexityDistribution = new RangeDistributionBuilder(FILES_DISTRIB_BOTTOM_LIMITS);
    complexityDistribution.add(squidFile.getDouble(ErlangMetric.COMPLEXITY));
    NewMeasure<Serializable> m = context.newMeasure();
    m.forMetric(metricFinder.findByKey(CoreMetrics.FILE_COMPLEXITY_DISTRIBUTION_KEY))
            .on(sonarFile)
            .withValue(complexityDistribution.build())
            .save();
  }

  private void saveViolations(SensorContext context, InputFile sonarFile, SourceFile squidFile) {
    Collection<CheckMessage> messages = squidFile.getCheckMessages();
    if (messages != null) {
      for (CheckMessage message : messages) {
        RuleKey ruleKey = checks.ruleKey(message.getCheck());
        TextRange range = sonarFile.selectLine(message.getLine());
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
