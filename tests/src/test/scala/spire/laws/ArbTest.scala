package spire
package laws

import scala.util.Random

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.FunSuite
import spire.math.interval._
import spire.math.{Interval, SafeLong, Rational}

class ArbTest extends FunSuite {

  // fixed random seed
  def seed: Seed = Seed(0L)

  def params: Gen.Parameters = Gen.Parameters.default

  test("arb.rational") {
    // generate a reasonably large number of rationals and check if we get each kind of rational at least once
    val samples = Array.fill(100)(spire.laws.arb.rational.arbitrary(params, seed)).flatten
    def classify(x: Rational): String = {
      if (x.isZero) "zero"
      else if (x.isOne) "one"
      else if (x.isValidLong) "long"
      else if (x.isWhole) "big"
      else {
        if (x.numerator.isValidLong && x.denominator.isValidLong) {
          if (x.numerator == 1) "1/long"
          else "long/long"
        } else {
          if (x.numerator == 1) "1/big"
          else if (x.numerator.isValidLong) "long/big"
          else if (x.denominator.isValidLong) "big/long"
          else "big/big"
        }
      }
    }
    val kinds = samples.map(classify).toSet
    val expected = Set("zero", "one", "long", "big", "1/long", "1/big", "long/big", "long/long", "big/long", "big/big")
    val missing = expected diff kinds
    assert(missing.isEmpty)
  }

  test("arb.safeLong") {
    val samples = Array.fill(100)(spire.laws.arb.safeLong.arbitrary(params, seed)).flatten
    def classify(x: SafeLong): String = {
      if (x.isZero) "zero"
      else if (x.isOne) "one"
      else if (x.isValidLong) "long"
      else "big"
    }
    val kinds = samples.map(classify).distinct.sorted
    assert(kinds.length == 4)
  }

  test("arb.interval") {
    import spire.std.int._
    val samples = Array.fill(100)(spire.laws.arb.interval[Int].arbitrary(params, seed)).flatten
    def classify(x: Interval[Int]): String = x.fold {
      case (Unbound(), Unbound()) ⇒ "all"
      case (Unbound(), Open(_)) ⇒ ")"
      case (Unbound(), Closed(_)) ⇒ "]"
      case (Open(_), Unbound()) ⇒ "("
      case (Closed(_), Unbound()) ⇒ "["
      case (Open(_), Open(_)) ⇒ "()"
      case (Open(_), Closed(_)) ⇒ "(]"
      case (Closed(_), Open(_)) ⇒ "[)"
      case (Closed(a), Closed(b)) ⇒ if(a!=b) "[]" else "point"
      case _ ⇒ "empty"
    }
    val kinds = samples.map(classify).distinct.sorted
    assert(kinds.length == 11)
  }
}
