/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.std.int._
import spire.math._

class LiteralsSuite extends munit.FunSuite {
  test("byte literals") {
    import spire.syntax.literals._
    assertEquals(b"-128", (-128: Byte))
    assertEquals(b"-100", (-100: Byte))
    assertEquals(b"0", (0: Byte))
    assertEquals(b"100", (100: Byte))
    assertEquals(b"127", (127: Byte))
    assertEquals(b"128", (-128: Byte))
    assertEquals(b"255", (-1: Byte))
    assert(compileErrors("""b"256"""").contains("illegal constant: 256"))
  }

  test("short literals") {
    import spire.syntax.literals._
    assertEquals(h"-32768", (-32768: Short))
    assertEquals(h"-10000", (-10000: Short))
    assertEquals(h"0", (0: Short))
    assertEquals(h"10012", (10012: Short))
    assertEquals(h"32767", (32767: Short))
    assertEquals(h"32768", (-32768: Short))
    assertEquals(h"65535", (-1: Short))
    assert(compileErrors("""h"65536"""").contains("illegal constant: 65536"))
  }

  test("int operators") {
    import spire.syntax.all._
    assertEquals((5 ** 2), 25)
    assertEquals((5 /~ 2), 2)
    assertEquals((5 /% 2), ((2, 1)))
    assertEquals(25.sqrt, 5)
  }

  test("inter-type operators") {
    import spire.std.double._
    val c = Complex(2.0, 3.0)
    val q = Rational(4, 5)
    val r = Algebraic(3.0)

    assertEquals(c + 1, Complex(3.0, 3.0))
    assertEquals(1 + c, Complex(3.0, 3.0))

    assertEquals(q + 1, Rational(9, 5))
    assertEquals(1 + q, Rational(9, 5))

    assertEquals(r + 1, Algebraic(4.0))
    assertEquals(1 + r, Algebraic(4.0))
  }

  test("unsigned literals") {
    import spire.syntax.literals._
    assertEquals(ub"1", UByte(1))
    assertEquals(ub"255", UByte(-1))
    assertEquals(ub"120", UByte(120))
    assert(compileErrors("""ub"256"""").contains("illegal constant: 256"))

    assertEquals(uh"1", UShort(1))
    assertEquals(uh"65535", UShort(65535))
    assertEquals(uh"120", UShort(120))
    assert(compileErrors("""uh"65536"""").contains("illegal constant: 65536"))

    assertEquals(ui"1", UInt(1))
    assertEquals(ui"65535", UInt(65535))
    assertEquals(ui"120", UInt(120))
    assert(compileErrors("""ui"-1"""").contains("illegal constant: -1"))

    assertEquals(ul"1", ULong(1))
    assertEquals(ul"65535", ULong(65535))
    assertEquals(ul"120", ULong(120))
    assert(compileErrors("""ul"-1"""").contains("illegal constant: -1"))
  }
}
