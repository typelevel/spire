package spire
package util

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.check.ScalaCheckDrivenPropertyChecks

import java.nio.ByteBuffer

class PackCheck extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  import Pack.{intToByte, longToByte}

  property("int <=> bytes") {
    forAll { (n: Int) =>
      val bs0 = Pack.intToBytes(n)
      val Array(b0, b1, b2, b3) = bs0

      val bs1 = Array(intToByte(n)(0), intToByte(n)(1), intToByte(n)(2), intToByte(n)(3))

      val i = 0
      val bs2 = Array(intToByte(n)(i+0), intToByte(n)(i+1), intToByte(n)(i+2), intToByte(n)(i+3))

      val n1 = Pack.intFromBytes(bs0)
      val n2 = Pack.intFromBytes(bs1)
      val n3 = Pack.intFromBytes(b0, b1, b2, b3)
      val n4 = Pack.intFromByteBuffer(ByteBuffer.wrap(bs0))
      val n5 = Pack.intFromBytes(bs2)
      n shouldBe n1
      n shouldBe n2
      n shouldBe n3
      n shouldBe n4
      n shouldBe n5
    }
  }

  property("ints <=> bytes") {
    forAll {
      (ns: Array[Int]) =>
        val bs = Pack.intsToBytes(ns)
        val ns1 = Pack.intsFromBytes(bs, ns.length)
        val ns2 = Pack.intsFromByteBuffer(ByteBuffer.wrap(bs), ns.length)
        ns shouldBe ns1
        ns shouldBe ns2
    }
  }

  property("long <=> bytes") {
    forAll { (n: Long) =>
      val bs0 = Pack.longToBytes(n)
      val Array(b0, b1, b2, b3, b4, b5, b6, b7) = bs0
      val bs1 = Array(
        longToByte(n)(0), longToByte(n)(1), longToByte(n)(2), longToByte(n)(3),
        longToByte(n)(4), longToByte(n)(5), longToByte(n)(6), longToByte(n)(7)
      )

      val i = 0
      val bs2 = Array(
        longToByte(n)(i+0), longToByte(n)(i+1), longToByte(n)(i+2), longToByte(n)(i+3),
        longToByte(n)(i+4), longToByte(n)(i+5), longToByte(n)(i+6), longToByte(n)(i+7)
      )

      val n1 = Pack.longFromBytes(bs0)
      val n2 = Pack.longFromBytes(bs1)
      val n3 = Pack.longFromBytes(b0, b1, b2, b3, b4, b5, b6, b7)
      val n4 = Pack.longFromByteBuffer(ByteBuffer.wrap(bs0))
      val n5 = Pack.longFromBytes(bs2)
      n shouldBe n1
      n shouldBe n2
      n shouldBe n3
      n shouldBe n4
      n shouldBe n5
    }
  }

  property("longs <=> bytes") {
    forAll {
      (ns: Array[Long]) =>
        val bs = Pack.longsToBytes(ns)
        val ns1 = Pack.longsFromBytes(bs, ns.length)
        val ns2 = Pack.longsFromByteBuffer(ByteBuffer.wrap(bs), ns.length)
        ns shouldBe ns1
        ns shouldBe ns2
    }
  }
}
