package spire.random

import org.scalatest.Matchers
import org.scalatest._
import prop._

import org.scalacheck._

class SamplingTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  val rng = spire.random.rng.Lcg64.fromTime()

  val Size = 100
  val range = (1 to Size)
  val ns = range.toArray
  val gen = Gen.chooseNum(1, Size)

  def verify(result: Array[Int], n: Int): Unit = {
    result.toSet.size shouldBe n
    result.toSet.forall(range.contains) shouldBe true
  }

  property("sampleArray(ns, [1, n])") {
    forAll(gen)((n: Int) => verify(rng.sampleFromArray(ns, n), n))
  }

  property("sampleTraversable(ns, [1, n])") {
    forAll(gen)((n: Int) => verify(rng.sampleFromTraversable(ns, n), n))
  }
}
