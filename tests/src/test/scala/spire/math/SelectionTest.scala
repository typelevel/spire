package spire
package math

import spire.algebra._
import spire.std.int._

import scala.reflect.ClassTag

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers


trait SelectTest extends FunSuite /* with Checkers */ {
  def selector: Select

  final def select[@spec A: Order: ClassTag](data: Array[A], k: Int) =
    selector.select(data, k)

  def shuffle[A: ClassTag](as: Array[A]): Array[A] =
    scala.util.Random.shuffle(as.toList).toArray

  test("selection in 0-length array") {
    // Shouldn't throw an exception.
    select(new Array[Int](0), 0)
  }

  test("select in 1-length array") {
    val as = Array(1)
    select(as, 0)
    assert(as(0) === 1)
  }

  test("select from multiple equal elements") {
    val as = Array(0, 0, 1, 1, 2, 2)
    select(as, 0); assert(as(0) === 0)
    select(as, 1); assert(as(1) === 0)
    select(as, 2); assert(as(2) === 1)
    select(as, 3); assert(as(3) === 1)
    select(as, 4); assert(as(4) === 2)
    select(as, 5); assert(as(5) === 2)
  }

  test("arbitrary selection") {
    (1 to 10) foreach { len =>
      val as = Array.range(0, len)

      (0 until len) foreach { i =>
        (1 to 5) foreach { _ =>
          val bs = shuffle(as)
          val orig = bs.clone()
          select(bs, i)
          assert(bs(i) === i, "Select %d on %s failed." format (i, orig.mkString("[ ", ", ", " ]")))
        }
      }
    }
  }
}

class LinearSelectTest extends SelectTest {
  val selector = LinearSelect
}

class QuickSelectTest extends SelectTest {
  val selector = QuickSelect
}

