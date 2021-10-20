/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package syntax

import scala.collection.mutable

class FastForSuite extends munit.FunSuite {

  import spire.syntax.fastFor._

  test("simple fastFor") {
    val l = mutable.ListBuffer[Int]()
    fastFor(0)(_ < 5, _ + 1) { x =>
      l.append(x)
    }
    assertEquals(l.toList, List(0, 1, 2, 3, 4))
  }

  test("nested fastFor") {
    val s = mutable.Set.empty[Int]
    fastFor(0)(_ < 10, _ + 1) { x =>
      fastFor(10)(_ < 100, _ + 10) { y =>
        s.add(x + y)
      }
    }
    assertEquals(s.toSet, (10 to 99).toSet)
  }

  test("symbol collision fastFor") {
    val b = mutable.ArrayBuffer.empty[Int]
    fastFor(0)(_ < 3, _ + 1) { x =>
      fastFor(0)(_ < 2, _ + 1) { y =>
        val x = y
        b += x
      }
    }
    assertEquals(b.toList, List(0, 1, 0, 1, 0, 1))
  }

  test("functions with side effects in fastFor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    fastFor(0)({ v += 1; _ < 3 }, { v += 10; _ + 1 }) {
      v += 100
      x => {
        b += x
      }
    }
    assertEquals(v, 111)
    assertEquals(b.toList, List(0, 1, 2))
  }

  test("functions with side effects function values in fastFor") {
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
    fastFor(0)(test, incr)(body)
    assertEquals(v, 111)
    assertEquals(b.toList, List(0, 1, 2))
  }

  test("functions with side effects function by-value params in fastFor") {
    val b = mutable.ArrayBuffer.empty[Int]
    var v = 0
    def run(test: => (Int => Boolean), incr: => (Int => Int), body: => (Int => Unit)): Unit = {
      fastFor(0)(test, incr)(body)
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

  // This test distinguishes fastFor from cfor
  test("doesn't capture value in closure") {
    val b1 = collection.mutable.ArrayBuffer.empty[() => Int]
    fastFor(0)(_ < 3, _ + 1) { x =>
      b1 += (() => x)
    }
    val b2 = collection.mutable.ArrayBuffer[() => Int]()
    (0 until 3).foreach { x =>
      b2 += (() => x)
    }
    assertEquals(b1.map(_.apply()).toList, b2.map(_.apply()).toList)
  }

  test("capture value in inner class") {
    val b = collection.mutable.ArrayBuffer[Int]()
    fastFor(0)(_ < 3, _ + 1) { x =>
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
    fastFor(0)(_ < arr.length, _ + 1) { i =>
      val (a, b) = arr(i)
      t += a + 2 * b
    }
    assertEquals(t, 27)
  }

  test("destructure tuples") {
    var t = 0
    fastFor((0, 0))(_._1 < 3, t => (t._1 + 1, t._2 + 2)) { case (a, b) =>
      t += 3 * a + b
    }
    assertEquals(t, 15)
  }

  test("fastForRange(1 until 4)") {
    var t = 0
    fastForRange(1 until 4) { x =>
      t += x
    }
    assertEquals(t, 6)
  }

  test("fastForRange(0 to 10 by 2)") {
    var t = 0
    fastForRange(0 to 10 by 2) { x =>
      t += x
    }
    assertEquals(t, 30)
  }

  test("fastForRange(3 to 1 by -1)") {
    var t = 0
    fastForRange(3 to 1 by -1) { x =>
      t += x
    }
    assertEquals(t, 6)
  }

  test("fastForRange(0 to 0 by -1)") {
    var t = 0
    fastForRange(0 to 0 by -1) { x =>
      t += 1
    }
    assertEquals(t, 1)
  }
}
