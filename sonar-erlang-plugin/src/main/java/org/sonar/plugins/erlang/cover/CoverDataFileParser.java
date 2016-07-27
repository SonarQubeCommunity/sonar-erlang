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
package org.sonar.plugins.erlang.cover;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpInputStream;
import com.google.common.base.Preconditions;
import com.google.common.io.Closeables;

import java.io.BufferedInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
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
public class CoverDataFileParser {

  private static OtpErlangAtom SIZE_ATOM = new OtpErlangAtom("$size");
  private static OtpErlangAtom BUMP_ATOM = new OtpErlangAtom("bump");
  private static OtpErlangAtom FILE_ATOM = new OtpErlangAtom("file");

  /**
   * Do not instantiate CoverCoverageParser.
   */
  private CoverDataFileParser() {
  }

  public static List<ErlangFileCoverage> parse(File inFile) throws IOException {
    InputStream fin = new FileInputStream(inFile);
    try {
      InputStream in = new BufferedInputStream(fin);
      try {
        return parse(in);
      } finally {
        Closeables.closeQuietly(in);
      }
    } finally {
      Closeables.closeQuietly(fin);
    }
  }

  public static List<ErlangFileCoverage> parse(InputStream in) throws IOException {
    Preconditions.checkNotNull(in);
    List<ErlangFileCoverage> ret = new ArrayList<ErlangFileCoverage>();

    try {

      OtpErlangObject term;
      ErlangFileCoverage moduleResult = null;
      while ((term = readTerm(in)) != null) {
        if (term instanceof OtpErlangTuple) {
          OtpErlangTuple tuple = (OtpErlangTuple) term;
          if (tuple.arity() == 3 && FILE_ATOM.equals(tuple.elementAt(0))) {
            // {file,sip_ua_client,"/Users/idubrov/Projects/siperl/apps/sip/ebin/sip_ua_client.beam"}
            String module = eatom(tuple, 1);

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

  private static String eatom(OtpErlangTuple tuple, int pos) {
    return ((OtpErlangAtom) tuple.elementAt(pos)).atomValue();
  }

  private static int eint(OtpErlangTuple tuple, int pos) throws OtpErlangRangeException {
    return ((OtpErlangLong) tuple.elementAt(pos)).intValue();
  }

  /**
   * Java implementation of cover:get_term/1
   *
   * @param in
   * @return
   * @throws IOException
   * @throws OtpErlangDecodeException
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
    OtpInputStream ein = new FixedOtpInputStream(buf);
    OtpErlangObject term = ein.read_any();
    Closeables.closeQuietly(ein);
    return term;
  }
}
