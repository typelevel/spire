package spire.buffer

import org.scalatest.FunSuite

class BufferTest extends FunSuite {

  def basicBufferTests(data:Array[Int], b:Buffer[Int]) {
    assert(data.length > 4)
    assert(b.length === data.length)
    assert(b.toArray === data)

    data.zipWithIndex.foreach{ case (x, i) => assert(b(i) === x) }
    data.zipWithIndex.foreach{ case (x, i) => assert(b.get(i) === Some(x)) }

    val s = b.slice(2, 4)
    assert(s.length === 2)
    assert(s.toArray === Array(data(2), data(3)))
  }

  test("Immutable[Int]") {
    val data = Array(1,2,3,4,5,6,7,8)
    val b = Immutable(data)
    basicBufferTests(data, b)
  }

  test("safe Mutable[Int]") {
    val orig = Array(1,2,3,4,5,6,7,8)
    val data = orig.clone
    val m = Mutable.safe(data)

    basicBufferTests(data, m)

    m(0) = 999
    assert(m(0) === 999)
    assert(data(0) === orig(0))
  }

  test("unsafe Mutable[Int]") {
    val orig = Array(1,2,3,4,5,6,7,8)
    val data = orig.clone
    val m = Mutable.unsafe(data)

    basicBufferTests(data, m)

    m(0) = 999
    assert(m(0) === 999)
    assert(data(0) === 999)
  }
}
