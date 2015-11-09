package spire
package example

import spire.implicits._

class Loops {
  def nested(): Unit = {
    cfor(0)(_ < 5, _ + 1) {
      y => cfor(0)(_ < 3, _ + 1) { x =>
        println((x, y))
      }
    }
  }

  def simple(): Unit = {
    cfor(0)(_ < 10, _ + 1) { i => println(i) }
  }

  def simplew(): Unit = {
    var i = 0
    while (i < 10) {
      println(i)
      i += 1
    }
  }

  def simplet(): Unit = {
    @tailrec def loop(i: Int): Unit = {
      if (i < 10) {
        println(i)
        loop(i + 1)
      }
    }
    loop(0)
  }
}
