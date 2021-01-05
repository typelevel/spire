package spire
package math

import spire.std.int._

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
  }

  test("illegal byte literals") {
    import spire.macros._
    def tryit(s: String) = Macros.parseNumber(s, BigInt(-128), BigInt(255))
    assertEquals(tryit("-129"), Left("illegal constant: -129"))
    assertEquals(tryit("256"), Left("illegal constant: 256"))
    assertEquals(tryit("10000"), Left("illegal constant: 10000"))
    assertEquals(tryit("abc"), Left("illegal constant: abc"))
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
  }

  test("int operators") {
    import spire.syntax.std.int._
    import spire.syntax.nroot._
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
}
