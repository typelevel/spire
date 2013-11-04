package spire.math

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import java.nio.ByteBuffer
import spire.util.Pack

class PackCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {
  property("int <=> bytes") {
    forAll { (n: Int) =>
      val bs = Pack.intToBytes(n)
      val Array(b0, b1, b2, b3) = bs

      val n1 = Pack.intFromBytes(bs)
      val n2 = Pack.intFromBytes(b0, b1, b2, b3)
      val n3 = Pack.intFromByteBuffer(ByteBuffer.wrap(bs))
      n should be === n1
      n should be === n2
      n should be === n3
    }
  }

  property("ints <=> bytes") {
    forAll { (ns: Array[Int]) =>
      val bs = Pack.intsToBytes(ns)
      val ns1 = Pack.intsFromBytes(bs, ns.length)
      val ns2 = Pack.intsFromByteBuffer(ByteBuffer.wrap(bs), ns.length)
      ns should be === ns1
      ns should be === ns2
    }
  }

  property("long <=> bytes") {
    forAll { (n: Long) =>
      val bs = Pack.longToBytes(n)
      val Array(b0, b1, b2, b3, b4, b5, b6, b7) = bs

      val n1 = Pack.longFromBytes(bs)
      val n2 = Pack.longFromBytes(b0, b1, b2, b3, b4, b5, b6, b7)
      val n3 = Pack.longFromByteBuffer(ByteBuffer.wrap(bs))
      n should be === n1
      n should be === n2
      n should be === n3
    }
  }

  property("longs <=> bytes") {
    forAll { (ns: Array[Long]) =>
      val bs = Pack.longsToBytes(ns)
      val ns1 = Pack.longsFromBytes(bs, ns.length)
      val ns2 = Pack.longsFromByteBuffer(ByteBuffer.wrap(bs), ns.length)
      ns should be === ns1
      ns should be === ns2
    }
  }
}
