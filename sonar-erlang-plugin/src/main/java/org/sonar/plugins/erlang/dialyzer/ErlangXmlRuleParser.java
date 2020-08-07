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
package org.sonar.plugins.erlang.dialyzer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

final class ErlangXmlRuleParser {

  private static final Logger LOG = LoggerFactory.getLogger("ErlangXmlRuleParser");

  List<ErlangRule> parse(InputStream input) {
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
