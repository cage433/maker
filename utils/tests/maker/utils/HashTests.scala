/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.utils

import org.scalatest.FunSuite

class HashTests extends FunSuite {

  test("fromHex should support output of hashWithComment") {
    val hex = "37bf3dd962ed30114d264a69563f595e-comment"
    val hash = Hash.fromHex(hex)
    assert(hash.bytes.length === 16)
    assert(hex === hash.hashWithComment)
    assert(hash.comment == "comment")
  }

  test("hash equals") {
    val a1 = Hash.fromHex("37bf3dd962ed30114d264a69563f595e-comment") // different instances
    val a2 = Hash.fromHex("37bf3dd962ed30114d264a69563f595e-comment") // of the same hash
    val b  = Hash.fromHex("717d1bb505d6a92ec7425184e3a6c81f-comment")
    assert(if (a1 == a2) None else Some(a1 + " != " + a1)) //=== uses special array equals
    assert(a1.hashCode == a2.hashCode)
    assert(if (a1 != b) None else Some(a1 + " == " + b))
  }
}
