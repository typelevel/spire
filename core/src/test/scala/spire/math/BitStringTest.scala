package spire.math

import org.scalatest.FunSuite
import spire.implicits.{eqOps => _, _}

class BitStringTest extends FunSuite {

  case class I[A: BitString](b: A, bc: Int, hob: A, lob: A, nlz: Int, ntz: Int)

  def testCases[A: BitString](cases: List[I[A]]): Unit =
    cases.foreach {
      case I(b, bc, hob, lob, nlz, ntz) =>
      assert(b.bitCount == bc)
      assert(b.highestOneBit === hob)
      assert(b.lowestOneBit === lob)
      assert(b.numberOfLeadingZeros === nlz)
      assert(b.numberOfTrailingZeros === ntz)
    }

  test("BitString[Byte]") {
    import spire.syntax.literals._
    testCases(I(b"0", 0, b"0", b"0", 8, 8) ::
      I(b"7", 3, b"4", b"1", 5, 0) ::
      I(b"62", 5, b"32", b"2", 2, 1) ::
      I(b"127", 7, b"64", b"1", 1, 0) ::
      I(b"128", 1, b"128", b"128", 0, 7) ::
      I(b"255", 8, b"128", b"1", 0, 0) ::
      Nil)
  }

  test("BitString[Short]") {
    import spire.syntax.literals._
    testCases(I(h"0", 0, h"0", h"0", 16, 16) ::
      I(h"7", 3, h"4", h"1", 13, 0) ::
      I(h"62", 5, h"32", h"2", 10, 1) ::
      I(h"127", 7, h"64", h"1", 9, 0) ::
      I(h"128", 1, h"128", h"128", 8, 7) ::
      I(h"255", 8, h"128", h"1", 8, 0) ::
      I(h"256", 1, h"256", h"256", 7, 8) ::
      I(h"32767", 15, h"16384", h"1", 1, 0) ::
      I(h"65535", 16, h"32768", h"1", 0, 0) ::
      Nil)
  }

  test("BitString[Int]") {
    testCases(I(0, 0, 0, 0, 32, 32) ::
      I(7, 3, 4, 1, 29, 0) ::
      I(62, 5, 32, 2, 26, 1) ::
      I(127, 7, 64, 1, 25, 0) ::
      I(128, 1, 128, 128, 24, 7) ::
      I(255, 8, 128, 1, 24, 0) ::
      I(256, 1, 256, 256, 23, 8) ::
      I(32767, 15, 16384, 1, 17, 0) ::
      I(65535, 16, 32768, 1, 16, 0) ::
      Nil)
  }
}
