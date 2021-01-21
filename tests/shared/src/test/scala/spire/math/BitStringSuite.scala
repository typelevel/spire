package spire
package math

import spire.implicits._

class BitStringSuite extends munit.FunSuite {

  case class I[A: BitString](b: A, bc: Int, hob: A, lob: A, nlz: Int, ntz: Int)

  def testCases[A: BitString](cases: List[I[A]]): Unit =
    cases.foreach { expected =>
      val b = expected.b
      val found = I(
        b,
        b.bitCount,
        b.highestOneBit,
        b.lowestOneBit,
        b.numberOfLeadingZeros,
        b.numberOfTrailingZeros
      )
      assertEquals(found, expected)
    }

  test("BitString[Byte]") {
    import spire.syntax.literals._
    testCases(
      I(b"0", 0, b"0", b"0", 8, 8) ::
        I(b"7", 3, b"4", b"1", 5, 0) ::
        I(b"62", 5, b"32", b"2", 2, 1) ::
        I(b"127", 7, b"64", b"1", 1, 0) ::
        I(b"-128", 1, b"-128", b"-128", 0, 7) ::
        I(b"-1", 8, b"-128", b"1", 0, 0) ::
        Nil
    )
  }

  test("BitString[Short]") {
    import spire.syntax.literals._
    testCases(
      I(h"0", 0, h"0", h"0", 16, 16) ::
        I(h"7", 3, h"4", h"1", 13, 0) ::
        I(h"62", 5, h"32", h"2", 10, 1) ::
        I(h"127", 7, h"64", h"1", 9, 0) ::
        I(h"128", 1, h"128", h"128", 8, 7) ::
        I(h"255", 8, h"128", h"1", 8, 0) ::
        I(h"256", 1, h"256", h"256", 7, 8) ::
        I(h"23985", 9, h"16384", h"1", 1, 0) ::
        I(h"32767", 15, h"16384", h"1", 1, 0) ::
        I(h"-32768", 1, h"-32768", h"-32768", 0, 15) ::
        I(h"-1", 16, h"32768", h"1", 0, 0) ::
        Nil
    )
  }

  test("BitString[Int]") {
    testCases(
      I(0, 0, 0, 0, 32, 32) ::
        I(7, 3, 4, 1, 29, 0) ::
        I(62, 5, 32, 2, 26, 1) ::
        I(127, 7, 64, 1, 25, 0) ::
        I(128, 1, 128, 128, 24, 7) ::
        I(255, 8, 128, 1, 24, 0) ::
        I(256, 1, 256, 256, 23, 8) ::
        I(23985, 9, 16384, 1, 17, 0) ::
        I(32767, 15, 16384, 1, 17, 0) ::
        I(65535, 16, 32768, 1, 16, 0) ::
        I(65535, 16, 32768, 1, 16, 0) ::
        I(293859800, 14, 268435456, 8, 3, 3) ::
        I(2147483647, 31, 1073741824, 1, 1, 0) ::
        I(-2147483648, 1, -2147483648, -2147483648, 0, 31) ::
        I(-1, 32, -2147483648, 1, 0, 0) ::
        Nil
    )
  }

  test("BitString[Long]") {
    testCases(
      I(0L, 0, 0L, 0L, 64, 64) ::
        I(7L, 3, 4L, 1L, 61, 0) ::
        I(62L, 5, 32L, 2L, 58, 1) ::
        I(127L, 7, 64L, 1L, 57, 0) ::
        I(128L, 1, 128L, 128L, 56, 7) ::
        I(255L, 8, 128L, 1L, 56, 0) ::
        I(256L, 1, 256L, 256L, 55, 8) ::
        I(23985L, 9, 16384L, 1L, 49, 0) ::
        I(32767L, 15, 16384L, 1L, 49, 0) ::
        I(65535L, 16, 32768L, 1L, 48, 0) ::
        I(65535L, 16, 32768L, 1L, 48, 0) ::
        I(293859800L, 14, 268435456L, 8L, 35, 3) ::
        I(2147483647L, 31, 1073741824L, 1L, 33, 0) ::
        I(2147483648L, 1, 2147483648L, 2147483648L, 32, 31) ::
        I(4294967295L, 32, 2147483648L, 1L, 32, 0) ::
        Nil
    )
  }

  def ls[A: BitString](n: A, i: Int): A = n << i
  def rs[A: BitString](n: A, i: Int): A = n >>> i
  def srs[A: BitString](n: A, i: Int): A = n >> i

  def eval[A](n: A)(f: (A, Int) => A): List[A] =
    List(f(n, 0), f(n, 1), f(n, 3), f(n, 4), f(n, 7))

  test("byte shifting") {
    import spire.syntax.literals._

    assertEquals(eval(b"1")(ls), List(b"1", b"2", b"8", b"16", b"-128"))
    assertEquals(eval(b"1")(rs), List(b"1", b"0", b"0", b"0", b"0"))
    assertEquals(eval(b"1")(srs), List(b"1", b"0", b"0", b"0", b"0"))

    assertEquals(eval(b"7")(ls), List(b"7", b"14", b"56", b"112", b"-128"))
    assertEquals(eval(b"7")(rs), List(b"7", b"3", b"0", b"0", b"0"))
    assertEquals(eval(b"7")(srs), List(b"7", b"3", b"0", b"0", b"0"))

    assertEquals(eval(b"127")(ls), List(b"127", b"-2", b"-8", b"-16", b"-128"))
    assertEquals(eval(b"127")(rs), List(b"127", b"63", b"15", b"7", b"0"))
    assertEquals(eval(b"127")(srs), List(b"127", b"63", b"15", b"7", b"0"))

    assertEquals(eval(b"-1")(ls), List(b"-1", b"-2", b"-8", b"-16", b"-128"))
    assertEquals(eval(b"-1")(rs), List(b"-1", b"127", b"31", b"15", b"1"))
    assertEquals(eval(b"-1")(srs), List(b"-1", b"-1", b"-1", b"-1", b"-1"))

    assertEquals(eval(b"-128")(ls), List(b"-128", b"0", b"0", b"0", b"0"))
    assertEquals(eval(b"-128")(rs), List(b"-128", b"64", b"16", b"8", b"1"))
    assertEquals(eval(b"-128")(srs), List(b"-128", b"-64", b"-16", b"-8", b"-1"))
  }
}

