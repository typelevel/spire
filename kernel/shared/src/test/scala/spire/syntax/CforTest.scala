package spire
package syntax

import scala.collection.mutable

import org.scalatest._
import prop._

class CforTest extends PropSpec with Matchers with PropertyChecks {

  override def convertToEqualizer[T](left: T): Equalizer[T] = ???

  import spire.syntax.cfor._

  property("simple cfor") {
    val l = mutable.ListBuffer[Int]()
    cfor(0)(_ < 5, _ + 1) { x =>
      l.append(x)
    }
    l.toList shouldBe List(0, 1, 2, 3, 4)
  }

  property("nested cfor") {
    val s = mutable.Set.empty[Int]
    cfor(0)(_ < 10, _ + 1) { x =>
      cfor(10)(_ < 100, _ + 10) { y =>
        s.add(x+y)
      }
    }
    s shouldBe (10 to 99).toSet
  }

  property("symbol collision cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    cfor(0)(_ < 3, _ + 1) { x =>
      cfor(0)(_ < 2, _ + 1) { y =>
        val x = y
        b += x
      }
    }
    b.toList shouldBe List(0, 1, 0, 1, 0, 1)
  }

  property("functions with side effects in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    cfor(0)({ v += 1; _ < 3 }, { v += 10; _ + 1})({
      v += 100
      x => {
        b += x
      }
    })
    v shouldBe 111
    b.toList shouldBe List(0, 1, 2)
  }

  property("functions with side effects function values in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    def test: Int => Boolean = { v += 1; _ < 3 }
    def incr: Int => Int = { v += 10; _ + 1}
    def body: Int => Unit = {
      v += 100
      x => {
        b += x
      }
    }
    cfor(0)(test, incr)(body)
    v shouldBe 111
    b.toList shouldBe List(0, 1, 2)
  }

  property("functions with side effects function by-value params in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    def run(
        test: => (Int => Boolean),
        incr: => (Int => Int),
        body: => (Int => Unit)): Unit = {
      cfor(0)(test, incr)(body)
    }
    run(
      { v += 1; _ < 3 },
      { v += 10; _ + 1},
      {
        v += 100
        x => {
          b += x
        }
      }
    )
    v shouldBe 111
    b.toList shouldBe List(0, 1, 2)
  }

  property("capture value in closure") {
    val b1 = collection.mutable.ArrayBuffer.empty[() => Int]
    cfor(0)(_ < 3, _ + 1) { x =>
      b1 += (() => x)
    }
    val b2 = collection.mutable.ArrayBuffer[() => Int]()
    var i = 0
    while (i < 3) {
      b2 += (() => i)
      i += 1
    }
    b1.map(_.apply).toList shouldBe b2.map(_.apply).toList
  }

  property("capture value in inner class") {
    val b = collection.mutable.ArrayBuffer[Int]()
    cfor(0)(_ < 3, _ + 1) { x => {
        class A { def f = x }
        b += (new A().f)
    }}
    b.toList shouldBe List(0, 1, 2)
  }

  property("type tree bug fixed") {
    val arr = Array((1,2), (2,3), (4,5))
    var t = 0
    cfor(0)(_ < arr.length, _ + 1) { i =>
      val (a, b) = arr(i)
      t += a + 2 * b
    }
    t shouldBe 27
  }

  property("destructure tuples") {
    var t = 0
    cfor((0, 0))(_._1 < 3, t => (t._1 + 1, t._2 + 2)) { case (a, b) =>
      t += 3 * a + b
    }
    t shouldBe 15
  }

  property("cforRange(1 until 4)") {
    var t = 0
    cforRange(1 until 4) { x =>
      t += x
    }
    t shouldBe 6
  }

  property("cforRange(0 to 10 by 2)") {
    var t = 0
    cforRange(0 to 10 by 2) { x =>
      t += x
    }
    t shouldBe 30
  }

  property("cforRange(3 to 1 by -1)") {
    var t = 0
    cforRange(3 to 1 by -1) { x =>
      t += x
    }
    t shouldBe 6
  }

  property("cforRange(0 to 0 by -1)") {
    var t = 0
    cforRange(0 to 0 by -1) { x =>
      t += 1
    }
    t shouldBe 1
  }
}
