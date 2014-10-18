package spire.math.prime

import spire.math.SafeLong

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import spire.implicits._

class FactorHeapCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import SieveUtil._

  property("heap property") {
    forAll { (ns0: List[Int]) =>
      val ns = ns0.map(n => Factor(n, n))
      val sorted = ns.sorted.map(_.next)

      val h = new FactorHeap
      ns.foreach(h += _)
      h.size shouldBe ns.length

      var result = List.empty[SafeLong]
      while (h.nonEmpty) result = h.dequeue.next :: result

      result shouldBe sorted
    }
  }
}
