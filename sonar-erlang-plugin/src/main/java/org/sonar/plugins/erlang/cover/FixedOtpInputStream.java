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

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpExternal;
import com.ericsson.otp.erlang.OtpInputStream;
import com.google.common.io.Closeables;

import java.io.IOException;
import java.util.zip.Inflater;

/**
 * {@link OtpInputStream} with read_compressed fixed for Mac OS X.
 *
 * @see <a href="http://erlang.org/pipermail/erlang-patches/2009-September/000478.html">Bug+patch: jinterface, OtpInputStream.java</a>
 */
public class FixedOtpInputStream extends OtpInputStream {
  private final int flags;

  FixedOtpInputStream(byte[] buf) {
    super(buf);
    flags = 0;
  }

  public FixedOtpInputStream(byte[] buf, int flags) {
    super(buf, flags);
    this.flags = flags;
  }

  public FixedOtpInputStream(byte[] buf, int offset, int length, int flags) {
    super(buf, offset, length, flags);
    this.flags = flags;
  }

  @Override
  public OtpErlangObject read_compressed() throws OtpErlangDecodeException {
    final int tag = read1skip_version();

    if (tag != OtpExternal.compressedTag) {
      throw new OtpErlangDecodeException(
        "Wrong tag encountered, expected "
          + OtpExternal.compressedTag + ", got " + tag);
    }

    final int size = read4BE();
    final byte[] buf = new byte[size];
    final java.util.zip.InflaterInputStream is =
      new java.util.zip.InflaterInputStream(this, new Inflater(), size);
    try {
      final int dsize = is.read(buf, 0, size);
      if (dsize != size) {
        throw new OtpErlangDecodeException("Decompression gave "
          + dsize + " bytes, not " + size);
      }
      is.close();
    } catch (final IOException e) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }

    final OtpInputStream ois = new OtpInputStream(buf, flags);
    OtpErlangObject ret = ois.read_any();
    Closeables.closeQuietly(ois);
    return ret;
  }
}
