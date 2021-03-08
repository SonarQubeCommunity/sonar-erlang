package org.sonar.erlang.sslr.toolkit;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.sonar.colorizer.*;
import org.sonar.erlang.ErlangConfiguration;
import org.sonar.sslr.parser.ParserAdapter;

import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class ErlangConfigurationModelTest {

  ErlangConfigurationModel configurationModel;

  @Before
  public void setUp() {
    configurationModel = new ErlangConfigurationModel();
  }


  @Test
  public void getCharset() {
    // should return UTF-8 by default
    Assert.assertEquals(
            StandardCharsets.UTF_8,
            configurationModel.getCharset()
    );
  }

  @Test
  public void doGetParser() {
    Assert.assertEquals(
            ParserAdapter.class,
            configurationModel.doGetParser().getClass()
    );
  }

  @Test
  public void doGetTokenizers() {
    Set<Object> expectedTokenizers = Set.of(
            StringTokenizer.class,
            CDocTokenizer.class,
            JavadocTokenizer.class,
            CppDocTokenizer.class,
            KeywordsTokenizer.class);

    HashSet<Object> tokenizers = new HashSet<>(configurationModel
            .getTokenizers()
            .stream()
            .map(Tokenizer::getClass).collect(Collectors.toSet()));

    Assert.assertEquals(expectedTokenizers, tokenizers);
  }

  @Test
  public void getProperties() {
    Assert.assertEquals(
            "Charset",
            configurationModel.charsetProperty.getName()
    );
    Assert.assertEquals(
            "sonar.sourceEncoding",
            configurationModel.charsetProperty.getDescription()
    );
    // Assert that encoding is set to UTF-8 by default
    Assert.assertEquals(
            "UTF-8",
            configurationModel.charsetProperty.getValue()
    );
  }

  @Test
  public void getConfiguration() {
    Assert.assertEquals(
            new ErlangConfiguration(StandardCharsets.UTF_8).getCharset(),
            configurationModel.getConfiguration().getCharset()
    );
  }

  @Test
  public void getPropertyOrDefaultValue() {
    // Test when sourceEncoding property not set
    Assert.assertEquals(
            "UTF-8",
            ErlangConfigurationModel.getPropertyOrDefaultValue()
    );

    // Test when sourceEncoding property is set to some other encoding
    System.setProperty("sonar.sourceEncoding", "UTF-16");
    Assert.assertEquals(
            "UTF-16",
            ErlangConfigurationModel.getPropertyOrDefaultValue()
    );
  }

  @After
  public void cleanUp() {
    System.clearProperty("sonar.sourceEncoding");
  }
}
