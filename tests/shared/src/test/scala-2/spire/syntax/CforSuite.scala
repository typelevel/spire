package spire
package syntax

import scala.collection.mutable

class CforSuite extends munit.FunSuite {

  import spire.syntax.fastFor._

  test("capture value in closure") {
    val b1 = collection.mutable.ArrayBuffer.empty[() => Int]
    fastFor(0)(_ < 3, _ + 1) { x =>
      b1 += (() => x)
    // println(b1)
    }
    val b2 = collection.mutable.ArrayBuffer[() => Int]()
    var i = 0
    while (i < 3) {
      b2 += (() => i)
      i += 1
    }
    assertEquals(b1.map(_.apply()).toList, b2.map(_.apply()).toList)
  }

}
