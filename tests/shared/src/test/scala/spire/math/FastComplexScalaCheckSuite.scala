package spire
package math

import org.scalacheck.Prop._

class FastComplexScalaCheckSuite extends munit.ScalaCheckSuite {
  property("encode/decode") {
    forAll { (re: Float, im: Float) =>
      val n: Long = FastComplex.encode(re, im)
      val (r, i) = FastComplex.decode(n)

      if (r != re || i != im) {
        val rs = "%x".format(FastComplex.bits(re))
        val is = "%x".format(FastComplex.bits(im))
        val es = "%x".format(n)
        println(s"expected $rs $is got $es")
      }

      r == re && i == im
    }
  }
}
