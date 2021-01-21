package spire
package math.prime

import spire.math.SafeLong

import spire.implicits._
import org.scalacheck.Prop._

class FactorHeapScalaCheckSuite extends munit.ScalaCheckSuite {

  import SieveUtil._

  property("heap property") {
    forAll { (ns0: List[Int]) =>
      val ns = ns0.map(n => Factor(n, n))
      val sorted = ns.sorted.map(_.next)

      val h = new FactorHeap
      ns.foreach(h += _)
      val sizeCheck = h.size == ns.length

      var result = List.empty[SafeLong]
      while (h.nonEmpty) result = h.dequeue().next :: result

      sizeCheck && result == sorted
    }
  }
}
