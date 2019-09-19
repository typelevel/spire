package spire
package random

import org.scalacheck.Gen

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class ShufflingTest extends AnyPropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  val rng = spire.random.rng.Lcg64.fromTime()

  val range = Gen.chooseNum(1, 1000)

  property("shuffling doesn't change members") {
    forAll(range) { (n: Int) =>
      val ns1 = rng.generateInts(n)
      val ns2 = ns1.clone
      rng.shuffle(ns1)
      ns1.sorted shouldBe ns2.sorted
    }
  }
}
