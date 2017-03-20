package spire
package math

import java.math.BigInteger

import org.scalatest.FunSuite

import spire.algebra._
import spire.std.bigInt._
import spire.std.bigInteger._
import spire.syntax.isZ._

abstract class IsZTest[A:EuclideanRing:IsZ] extends FunSuite {

  def value(s: String): A = EuclideanRing[A].fromBigInt(BigInt(s))

  test("isPrime") {
    assert(value("11").isPrime)
    assert(value("971").isPrime)
    assert(value("10888869450418352160768000001").isPrime) // a factorial prime
    assert(value("2971215073").isPrime) // a Fibonacci prime
    assert(value("1111111111111111111").isPrime)
    assert(!value("46657").isPrime) // Poulet number
    assert(!value("52633").isPrime) // Poulet number
  }

  test("!") {
    assert(value("19").! === value("121645100408832000"))
    assert(value("14").! === value("87178291200"))
  }

  test("choose") {
    assert(value("3").choose(value("1")) === value("3"))
    assert(value("5").choose(value("3")) === value("10"))
  }
}

class SafeLongIsZTest extends IsZTest[SafeLong]

class BigIntIsZTest extends IsZTest[BigInt]

class BigIntegerIsZTest extends IsZTest[BigInteger]
