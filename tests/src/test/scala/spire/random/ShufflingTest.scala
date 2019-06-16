package spire
package random

import org.scalacheck.Gen

import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ShufflingTest extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
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
