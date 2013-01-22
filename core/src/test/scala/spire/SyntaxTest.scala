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
}
