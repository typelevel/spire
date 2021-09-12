package spire
package math

import spire.math.Rational

class LiteralSyntaxSuite extends munit.FunSuite {
  test("rationals") {
    import spire.syntax.literals._
    assertEquals(r"0", Rational(0))
    assertEquals(r"-1", Rational(-1))
    assertEquals(r"1", Rational(1))
    assertEquals(r"10/100", Rational(1, 10))
    assertEquals(r"-13/7", Rational(-13, 7))
    assertEquals(r"0/1", Rational(0))
    assertEquals(r"0/7", Rational(0))
    assertEquals(r"60/60", Rational(1))
    assertEquals(r"60/60", Rational(1))
    assertEquals(r"2/-3", Rational(-2, 3))
  }

  test("si literals") {
    import spire.syntax.literals.si._
    assertEquals(i"1 444 222 999", 1444222999)
    assertEquals(i"0", 0)
    assertEquals(i"-22 345", -22345)

    assertEquals(j"1 444 222 999", 1444222999L)
    assertEquals(j"0", 0L)
    assertEquals(j"-22 345", -22345L)
    assertEquals(j"-9 223 372 036 854 775 808", Long.MinValue)

    //   assertEquals(big"0", BigInt(0))
    //   assertEquals(big"1 000", BigInt(1000))
    //   assertEquals(big"-999 999 999 999 999 999 999 999 999", BigInt("-999999999999999999999999999"))
    //   assertEquals(big"1 000 000 000 000 000", BigInt("1000000000000000"))
    //
    //   assertEquals(dec"0", BigDecimal(0))
    //   assertEquals(dec"0.0", BigDecimal(0))
    //   assertEquals(dec"0.0", BigDecimal(0))
    //   assertEquals(dec"0.0000", BigDecimal(0))
    //   assertEquals(dec"0.1", BigDecimal("0.1"))
    //   assertEquals(dec"-0.998722", BigDecimal("-0.998722"))
    //   assertEquals(dec"1 000", BigDecimal(1000))
    //   assertEquals(dec"1 234 567.9913", BigDecimal("1234567.9913"))
    //   assertEquals(dec"1 000 000 000 000 000", BigDecimal("1000000000000000"))
  }
  //
  // test("us literals") {
  //   import spire.syntax.literals.us._
  //   assertEquals(i"1,444,222,999", 1444222999)
  //   assertEquals(i"0", 0)
  //   assertEquals(i"-22,345", -22345)
  //
  //   assertEquals(j"1,444,222,999", 1444222999L)
  //   assertEquals(j"0", 0L)
  //   assertEquals(j"-22,345", -22345L)
  //   assertEquals(j"-9,223,372,036,854,775,808", Long.MinValue)
  //
  //   assertEquals(big"0", BigInt(0))
  //   assertEquals(big"1,000", BigInt(1000))
  //   assertEquals(big"-999,999,999,999,999,999,999,999,999", BigInt("-999999999999999999999999999"))
  //   assertEquals(big"1,000,000,000,000,000", BigInt("1000000000000000"))
  //
  //   assertEquals(dec"0", BigDecimal(0))
  //   assertEquals(dec"0.0", BigDecimal(0))
  //   assertEquals(dec"0.0", BigDecimal(0))
  //   assertEquals(dec"0.0000", BigDecimal(0))
  //   assertEquals(dec"0.1", BigDecimal("0.1"))
  //   assertEquals(dec"-0.998722", BigDecimal("-0.998722"))
  //   assertEquals(dec"1,000", BigDecimal(1000))
  //   assertEquals(dec"1,234,567.9913", BigDecimal("1234567.9913"))
  //   assertEquals(dec"1,000,000,000,000,000", BigDecimal("1000000000000000"))
  // }
  //
  // test("eu literals") {
  //   import spire.syntax.literals.eu._
  //   assertEquals(i"1.444.222.999", 1444222999)
  //   assertEquals(i"0", 0)
  //   assertEquals(i"-22.345", -22345)
  //
  //   assertEquals(j"1.444.222.999", 1444222999L)
  //   assertEquals(j"0", 0L)
  //   assertEquals(j"-22.345", -22345L)
  //   assertEquals(j"-9.223.372.036.854.775.808", Long.MinValue)
  //
  //   assertEquals(big"0", BigInt(0))
  //   assertEquals(big"1.000", BigInt(1000))
  //   assertEquals(big"-999.999.999.999.999.999.999.999.999", BigInt("-999999999999999999999999999"))
  //   assertEquals(big"1.000.000.000.000.000", BigInt("1000000000000000"))
  //
  //   assertEquals(dec"0", BigDecimal(0))
  //   assertEquals(dec"0,0", BigDecimal(0))
  //   assertEquals(dec"0,0", BigDecimal(0))
  //   assertEquals(dec"0,0000", BigDecimal(0))
  //   assertEquals(dec"0,1", BigDecimal("0.1"))
  //   assertEquals(dec"-0,998722", BigDecimal("-0.998722"))
  //   assertEquals(dec"1.000", BigDecimal(1000))
  //   assertEquals(dec"1.234.567,9913", BigDecimal("1234567.9913"))
  //   assertEquals(dec"1.000.000.000.000.000", BigDecimal("1000000000000000"))
  // }
}
