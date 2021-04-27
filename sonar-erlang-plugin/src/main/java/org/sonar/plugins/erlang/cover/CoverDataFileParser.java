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
package org.sonar.plugins.erlang.cover;

import com.ericsson.otp.erlang.*;
import com.google.common.base.Preconditions;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Parse Erlang Cover data file.
 * <p/>
 * Binary file format is repetition of the following sections:
 * <code>
 * size (one byte).
 * term : &lt;size&gt; bytes.
 * If &lt;term&gt; is <code>{'$size', size2}</code>, then read next term of size &lt;size2&gt;
 * </code>
 * <p/>
 * Structure after deserializing:
 * <code>
 * [{file, Module, BeamFile} |
 * {Module, [{Module, Function, Arity, Clause, LinesCnt}]} |
 * {{bump, Module, Function, Arity, Clause, Line}, HitCnt}]
 * </code>
 *
 * @author idubrov
 * @author Tamas Kende
 */
class CoverDataFileParser {

  private static final OtpErlangAtom SIZE_ATOM = new OtpErlangAtom("$size");
  private static final OtpErlangAtom BUMP_ATOM = new OtpErlangAtom("bump");
  private static final OtpErlangAtom FILE_ATOM = new OtpErlangAtom("file");

  /**
   * Do not instantiate CoverCoverageParser.
   */
  private CoverDataFileParser() {
  }

  static List<ErlangFileCoverage> parse(File inFile) throws IOException {
    try (InputStream in = new BufferedInputStream(new FileInputStream(inFile))) {
      return parse(in);
    }
  }

  private static List<ErlangFileCoverage> parse(InputStream in) throws IOException {
    Preconditions.checkNotNull(in);
    List<ErlangFileCoverage> ret = new ArrayList<>();

    try {

      OtpErlangObject term;
      ErlangFileCoverage moduleResult = null;
      while ((term = readTerm(in)) != null) {
        if (term instanceof OtpErlangTuple) {
          OtpErlangTuple tuple = (OtpErlangTuple) term;
          if (tuple.arity() == 3 && FILE_ATOM.equals(tuple.elementAt(0))) {
            String module = eatom(tuple);

            moduleResult = new ErlangFileCoverage();
            moduleResult.setFilePath(module + ".erl");
            ret.add(moduleResult);

          } else if (tuple.arity() == 2 && tuple.elementAt(0) instanceof OtpErlangTuple) {
            // {{bump, Module, Function, Arity, Clause, Line}, HitCnt}]
            OtpErlangTuple tuple2 = (OtpErlangTuple) tuple.elementAt(0);
            if (tuple2.arity() == 6 && BUMP_ATOM.equals(tuple2.elementAt(0))) {
              int line = eint(tuple2, 5);
              int hits = eint(tuple, 1);

              // Ignore generated functions
              if (line != 0) {
                assert moduleResult != null;
                moduleResult.addLine(line, hits);
              }
            }
          }
        }
      }
    } catch (OtpErlangException e) {
      IOException exception = new IOException("File is not valid cover data file.");
      exception.setStackTrace(e.getStackTrace());
      throw exception;

    }
    return ret;
  }

  private static String eatom(OtpErlangTuple tuple) {
    return ((OtpErlangAtom) tuple.elementAt(1)).atomValue();
  }

  private static int eint(OtpErlangTuple tuple, int pos) throws OtpErlangRangeException {
    return ((OtpErlangLong) tuple.elementAt(pos)).intValue();
  }

  /**
   * Java implementation of cover:get_term/1
   *
   * @param in InputStream to read Erlang term from
   * @return OtpErlangObject Erlang term
   * @throws IOException              thrown if input stream read fails
   * @throws OtpErlangDecodeException thrown if Erlang term decoding fails
   */
  private static OtpErlangObject readTerm(InputStream in) throws IOException, OtpErlangException {
    int size = in.read();
    if (size == -1) {
      return null; // EOF
    }
    OtpErlangObject term = readTerm(in, size);

    // match {'$size', size2}
    if (term instanceof OtpErlangTuple) {
      OtpErlangTuple tuple = (OtpErlangTuple) term;
      if (tuple.arity() == 2 && SIZE_ATOM.equals(tuple.elementAt(0))) {
        OtpErlangLong erlangInt = (OtpErlangLong) tuple.elementAt(1);
        term = readTerm(in, erlangInt.intValue());
      }
    }
    return term;
  }

  private static OtpErlangObject readTerm(InputStream in, int size) throws IOException, OtpErlangDecodeException {
    byte[] buf = new byte[size];
    if (in.read(buf) != buf.length) {
      throw new EOFException("File is not valid cover data file.");
    }

    try (OtpInputStream ein = new FixedOtpInputStream(buf)) {
      return ein.read_any();
    }
  }
}
