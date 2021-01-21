package spire
package random

import java.util.Arrays
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ShufflingScalaCheckSuite extends munit.ScalaCheckSuite {
  val rng = spire.random.rng.Lcg64.fromTime()

  val range = Gen.chooseNum(1, 1000)

  property("shuffling doesn't change members") {
    forAll(range) { (n: Int) =>
      val ns1 = rng.generateInts(n)
      val ns2 = ns1.clone
      rng.shuffle(ns1)
      Arrays.equals(ns1.sorted, ns2.sorted)
    }
  }
}
