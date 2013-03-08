package spire.random

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import prop._

import org.scalacheck.Arbitrary._
import org.scalacheck._

class SamplingTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {
  val rng = spire.random.Lcg64.fromTime()

  val Size = 100
  val range = (1 to Size)
  val ns = range.toArray
  val gen = Gen.chooseNum(1, Size)

  def verify(result: Array[Int], n: Int) {
    result.toSet.size should be === n
    result.toSet.forall(range.contains) should be === true
  }

  property("sampleArray(ns, [1, n])") {
    forAll(gen)((n: Int) => verify(rng.sampleFromArray(ns, n), n))
  }

  property("sampleTraversable(ns, [1, n])") {
    forAll(gen)((n: Int) => verify(rng.sampleFromTraversable(ns, n), n))
  }
}
