package spire
package math

import java.util.Arrays

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import spire.implicits._

class BinaryMergeScalaCheckSuite extends munit.ScalaCheckSuite {

  implicit val arbitraryArray = implicitly[Arbitrary[Array[Int]]]

  property("merge") { forAll { (a:Array[Int], b:Array[Int]) =>
      val r = (a ++ b)
      Arrays.sort(a)
      Arrays.sort(b)
      Arrays.sort(r)
      val order = new CountingOrder[Int]
      val r1 = BinaryMerge.merge(a,b)(order, ClassTag.Int)
      r1.corresponds(r)(_ == _)
    }
  }

  property("merge order")  {
    forAll { (a:Array[Int], b:Array[Int]) =>
      val r = (a ++ b)
      Arrays.sort(a)
      Arrays.sort(b)
      Arrays.sort(r)
      val o1 = new CountingOrder[Int]
      val r1 = BinaryMerge.merge(a,b)(o1, ClassTag.Int)
      val o2 = new CountingOrder[Int]
      val r2 = BinaryMerge.merge(b,a)(o2, ClassTag.Int)
      r1.corresponds(r2)(_ == _)
    }
  }
}
