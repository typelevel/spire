package spire.util

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import java.nio.ByteBuffer

class PackCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {
  property("int <=> bytes") {
    forAll {
      (n: Int) =>
        val bs0 = Pack.intToBytes(n)
        val Array(b0, b1, b2, b3) = bs0
        val bs1 = Array(
          Pack.intToByte(n)(0), Pack.intToByte(n)(1),
          Pack.intToByte(n)(2), Pack.intToByte(n)(3)
        )

        val n1 = Pack.intFromBytes(bs0)
        val n2 = Pack.intFromBytes(bs1)
        val n3 = Pack.intFromBytes(b0, b1, b2, b3)
        val n4 = Pack.intFromByteBuffer(ByteBuffer.wrap(bs0))
        n should be === n1
        n should be === n2
        n should be === n3
        n should be === n4
    }
  }

  property("ints <=> bytes") {
    forAll {
      (ns: Array[Int]) =>
        val bs = Pack.intsToBytes(ns)
        val ns1 = Pack.intsFromBytes(bs, ns.length)
        val ns2 = Pack.intsFromByteBuffer(ByteBuffer.wrap(bs), ns.length)
        ns should be === ns1
        ns should be === ns2
    }
  }

  property("long <=> bytes") {
    forAll {
      (n: Long) =>
        val bs0 = Pack.longToBytes(n)
        val Array(b0, b1, b2, b3, b4, b5, b6, b7) = bs0
        val bs1 = Array(
          Pack.longToByte(n)(0), Pack.longToByte(n)(1),
          Pack.longToByte(n)(2), Pack.longToByte(n)(3),
          Pack.longToByte(n)(4), Pack.longToByte(n)(5),
          Pack.longToByte(n)(6), Pack.longToByte(n)(7)
        )

        val n1 = Pack.longFromBytes(bs0)
        val n2 = Pack.longFromBytes(bs1)
        val n3 = Pack.longFromBytes(b0, b1, b2, b3, b4, b5, b6, b7)
        val n4 = Pack.longFromByteBuffer(ByteBuffer.wrap(bs0))
        n should be === n1
        n should be === n2
        n should be === n3
        n should be === n4
    }
  }

  property("longs <=> bytes") {
    forAll {
      (ns: Array[Long]) =>
        val bs = Pack.longsToBytes(ns)
        val ns1 = Pack.longsFromBytes(bs, ns.length)
        val ns2 = Pack.longsFromByteBuffer(ByteBuffer.wrap(bs), ns.length)
        ns should be === ns1
        ns should be === ns2
    }
  }
}
