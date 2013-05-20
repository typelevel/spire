package spire.math

import org.scalatest.FunSuite

import spire.math._
import spire.std.int._

class LiteralsTest extends FunSuite {
  test("byte literals") {
    import spire.syntax.literals._
    assert(b"-128" === (-128:Byte))
    assert(b"-100" === (-100:Byte))
    assert(b"0" === (0:Byte))
    assert(b"100" === (100:Byte))
    assert(b"127" === (127:Byte))
    assert(b"128" === (-128:Byte))
    assert(b"255" === (-1:Byte))
  }

  test("illegal byte literals") {
    import spire.macrosk._
    def tryit(s: String) = Macros.parseNumber(s, BigInt(-128), BigInt(255))
    assert(tryit("-129") === Left("illegal constant: -129"))
    assert(tryit("256") === Left("illegal constant: 256"))
    assert(tryit("10000") === Left("illegal constant: 10000"))
    assert(tryit("abc") === Left("illegal constant: abc"))
  }

  test("short literals") {
    import spire.syntax.literals._
    assert(h"-32768" === (-32768:Short))
    assert(h"-10000" === (-10000:Short))
    assert(h"0" === (0:Short))
    assert(h"10012" === (10012:Short))
    assert(h"32767" === (32767:Short))
    assert(h"32768" === (-32768:Short))
    assert(h"65535" === (-1:Short))
  }

  test("int operators") {
    import spire.syntax.std.int._
    import spire.syntax.nroot._
    assert(5 ** 2 === 25)
    assert(5 /~ 2 === 2)
    assert(5 /% 2 === (2, 1))
    assert(25.sqrt === 5)
  }

  test("inter-type operators") {
    import spire.std.double._
    val c = Complex(2.0, 3.0)
    val q = Rational(4, 5)
    val r = Real(3.0)

    assert(c + 1 === Complex(3.0, 3.0))
    assert(1 + c === Complex(3.0, 3.0))

    assert(q + 1 === Rational(9, 5))
    assert(1 + q === Rational(9, 5))

    assert(r + 1 === Real(4.0))
    assert(1 + r === Real(4.0))
  }
}
