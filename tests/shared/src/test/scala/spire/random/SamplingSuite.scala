package spire
package random

import org.scalacheck.Gen
import org.scalacheck.Prop._

class SamplingSuite extends munit.ScalaCheckSuite {
  val rng = spire.random.rng.Lcg64.fromTime()

  val Size = 100
  val range = (1 to Size)
  val ns = range.toArray
  val gen = Gen.chooseNum(1, Size)

  def verify(result: Array[Int], n: Int): Boolean = {
    result.toSet.size == n && result.toSet.forall(range.contains)
  }

  property("sampleArray(ns, [1, n])") {
    forAll(gen)((n: Int) => verify(rng.sampleFromArray(ns, n), n))
  }

  property("sampleTraversable(ns, [1, n])") {
    forAll(gen)((n: Int) => verify(rng.sampleFromTraversable(ns, n), n))
  }
}
