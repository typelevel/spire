package spire.example

import spire.implicits._
import scala.annotation.tailrec

class Loops {
  def nested {
    cfor(0)(_ < 5, _ + 1) {
      y => cfor(0)(_ < 3, _ + 1) { x =>
        println((x, y))
      }
    }
  }
  
  def simple {
    cfor(0)(_ < 10, _ + 1) { i => println(i) }
  }

  def simplew {
    var i = 0
    while (i < 10) {
      println(i)
      i += 1
    }
  }

  def simplet {
    @tailrec def loop(i: Int) {
      if (i < 10) {
        println(i)
        loop(i + 1)
      }
    }
    loop(0)
  }
}
