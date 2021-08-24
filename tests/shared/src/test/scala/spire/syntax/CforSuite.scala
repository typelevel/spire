package spire
package syntax

import scala.collection.mutable

class CforSuite extends munit.FunSuite {

  import spire.syntax.cfor._

  test("simple cfor") {
    val l = mutable.ListBuffer[Int]()
    cfor(0)(_ < 5, _ + 1) { x =>
      l.append(x)
    }
    assertEquals(l.toList, List(0, 1, 2, 3, 4))
  }

  test("nested cfor") {
    val s = mutable.Set.empty[Int]
    cfor(0)(_ < 10, _ + 1) { x =>
      cfor(10)(_ < 100, _ + 10) { y =>
        s.add(x + y)
      }
    }
    assertEquals(s.toSet, (10 to 99).toSet)
  }

  test("symbol collision cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    cfor(0)(_ < 3, _ + 1) { x =>
      cfor(0)(_ < 2, _ + 1) { y =>
        val x = y
        b += x
      }
    }
    assertEquals(b.toList, List(0, 1, 0, 1, 0, 1))
  }

  test("functions with side effects in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    cfor(0)({ v += 1; _ < 3 }, { v += 10; _ + 1 }) {
      v += 100
      x => {
        b += x
      }
    }
    assertEquals(v, 111)
    assertEquals(b.toList, List(0, 1, 2))
  }

  test("functions with side effects function values in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    def test: Int => Boolean = { v += 1; _ < 3 }
    def incr: Int => Int = { v += 10; _ + 1 }
    def body: Int => Unit = {
      v += 100
      x => {
        b += x
      }
    }
    cfor(0)(test, incr)(body)
    assertEquals(v, 111)
    assertEquals(b.toList, List(0, 1, 2))
  }

  test("functions with side effects function by-value params in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    def run(test: => (Int => Boolean), incr: => (Int => Int), body: => (Int => Unit)): Unit = {
      cfor(0)(test, incr)(body)
    }
    run(
      { v += 1; _ < 3 },
      { v += 10; _ + 1 }, {
        v += 100
        x => {
          b += x
        }
      }
    )
    assertEquals(v, 111)
    assertEquals(b.toList, List(0, 1, 2))
  }

  // test("capture value in closure") {
  //   val b1 = collection.mutable.ArrayBuffer.empty[() => Int]
  //   cfor(0)(_ < 3, _ + 1) { x =>
  //     b1 += (() => x)
  //   // println(b1)
  //   }
  //   val b2 = collection.mutable.ArrayBuffer[() => Int]()
  //   var i = 0
  //   while (i < 3) {
  //     b2 += (() => i)
  //     i += 1
  //   }
  //   assertEquals(b1.map(_.apply()).toList, b2.map(_.apply()).toList)
  // }

  test("capture value in inner class") {
    val b = collection.mutable.ArrayBuffer[Int]()
    cfor(0)(_ < 3, _ + 1) { x =>
      {
        class A { def f = x }
        b += (new A().f)
      }
    }
    assertEquals(b.toList, List(0, 1, 2))
  }

  test("type tree bug fixed") {
    val arr = Array((1, 2), (2, 3), (4, 5))
    var t = 0
    cfor(0)(_ < arr.length, _ + 1) { i =>
      val (a, b) = arr(i)
      t += a + 2 * b
    }
    assertEquals(t, 27)
  }

  test("destructure tuples") {
    var t = 0
    cfor((0, 0))(_._1 < 3, t => (t._1 + 1, t._2 + 2)) { case (a, b) =>
      t += 3 * a + b
    }
    assertEquals(t, 15)
  }

  test("cforRange(1 until 4)") {
    var t = 0
    cforRange(1 until 4) { x =>
      t += x
    }
    assertEquals(t, 6)
  }

  test("cforRange(0 to 10 by 2)") {
    var t = 0
    cforRange(0 to 10 by 2) { x =>
      t += x
    }
    assertEquals(t, 30)
  }

  test("cforRange(3 to 1 by -1)") {
    var t = 0
    cforRange(3 to 1 by -1) { x =>
      t += x
    }
    assertEquals(t, 6)
  }

  test("cforRange(0 to 0 by -1)") {
    var t = 0
    cforRange(0 to 0 by -1) { x =>
      t += 1
    }
    assertEquals(t, 1)
  }
}
