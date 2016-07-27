/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012-2016 Tamas Kende
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
package org.sonar.plugins.erlang.dialyzer;

import org.dom4j.DocumentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.ServerComponent;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public final class ErlangXmlRuleParser implements ServerComponent {

  private static final Logger LOG = LoggerFactory.getLogger("ErlangXmlRuleParser");

  /**
   * Warning : the input stream is closed in this method
   *
   * @throws IOException
   * @throws SAXException
   * @throws ParserConfigurationException
   * @throws DocumentException
   */
  public List<ErlangRule> parse(InputStream input) {
    SAXParserFactory factory = SAXParserFactory.newInstance();
    SAXParser saxParser;
    RuleHandler a = new RuleHandler();
    try {
      saxParser = factory.newSAXParser();
      saxParser.parse(input, a);
    } catch (ParserConfigurationException e) {
      LOG.error("Error in configuration", e);
    } catch (SAXException e) {
      LOG.error("Error while parsing the Erlang's rules xml file", e);
    } catch (IOException e) {
      LOG.error("Error while reading the file", e);
    }

    return a.getRules();
  }
}
