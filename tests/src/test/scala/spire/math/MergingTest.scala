package spire
package math

import spire.implicits._
import org.scalatest.funsuite.AnyFunSuite
import spire.algebra.Order


class CountingOrder[T:Order] extends Order[T] {
  val wrapped = implicitly[Order[T]]
  var count = 0

  override def compare(x: T, y: T): Int = {
    count += 1
    wrapped.compare(x, y)
  }
}

class MergingTest extends AnyFunSuite {

  test("binary merge") {
    val a = Array.range(0,100).map(_ * 2)
    val b = Array.range(1,100).map(_ * 2)
    val o = new CountingOrder[Int]
    val r = BinaryMerge.merge(a,b)(o, ClassTag.Int)
    assert(r.sorted.corresponds(r)(_ == _))
    assert(o.count < 200)
  }

  test("linear merge") {
    val a = Array.range(0,100).map(_ * 2)
    val b = Array.range(1,100).map(_ * 3)
    val o = new CountingOrder[Int]
    val r1 = LinearMerge.merge(a,b)(o, ClassTag.Int)
    val r2 = LinearMerge.merge(b,a)(o, ClassTag.Int)
    assert(r1.sorted.corresponds(r1)(_ == _))
    assert(r2.sorted.corresponds(r2)(_ == _))
  }
}
