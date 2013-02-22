package spire

import spire.syntax._

import org.scalatest.FunSuite

import collection.mutable

class SyntaxTest extends FunSuite {
  test("simple cfor") {
    val l = mutable.ListBuffer[Int]()    
    cfor(0)(_ < 5, _ + 1) { x => 
      l.append(x)
    }
    assert(l.toSeq === Seq(0,1,2,3,4))
  }
  
  test("nested cfor") {
    val s = mutable.Set[Int]()
    cfor(0)(_ < 10, _ + 1) { x =>
      cfor(10)(_ < 100, _ + 10) { y =>
        s.add(x+y)
      }
    }
    assert(s.toSeq.sorted === (10 to 99))
  }
  
  test("symbol collision cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    cfor(0)(_ < 3, _ + 1) { x =>
      cfor(0)(_ < 2, _ + 1) { y => 
        val x = y
        b += x
      }
    }
    assert(b.toList === List(0, 1, 0, 1, 0, 1))
  }
  
  test("functions with side effects in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    cfor(0)({ v += 1; _ < 3 }, { v += 10; _ + 1})({
      v += 100
      x => {
        b += x
      }
    })
    assert(v == 111, s"v == $v")
    assert(b.toList === List(0, 1, 2), s"b.toList == ${b.toList}")
  }

  test("functions with side effects function values in cfor") {
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
    assert(v == 111, s"v == $v")
    assert(b.toList === List(0, 1, 2), s"b.toList == ${b.toList}")
  }

  test("functions with side effects function by-value params in cfor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    def run(
        test: => (Int => Boolean),
        incr: => (Int => Int),
        body: => (Int => Unit)) {
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
    assert(v == 111, s"v == $v")
    assert(b.toList === List(0, 1, 2), s"b.toList == ${b.toList}")
  }
  
  test("capture value in closure") {
    val b1 = collection.mutable.ArrayBuffer[() => Int]()
    cfor(0)(_ < 3, _ + 1) { x =>
      b1 += (() => x)
    }
    val b2 = collection.mutable.ArrayBuffer[() => Int]()
    var i = 0
    while (i < 3) {
      b2 += (() => i)
      i += 1
    }
    assert(b1.map(_.apply).toList === b2.map(_.apply).toList)
  }
  
  test("capture value in inner class") {
    val b = collection.mutable.ArrayBuffer[Int]()
    cfor(0)(_ < 3, _ + 1) { x => {
        class A { def f = x }
        b += (new A().f)
    }}
    assert(b.toList === List(0, 1, 2))
  }
}
