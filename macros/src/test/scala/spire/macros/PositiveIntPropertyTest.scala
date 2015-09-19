package spire.macros

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll

class PositiveIntPropertyTest extends Properties("PositiveInt")  {

  val positiveInteger: Gen[Int] = posNum[Int]
  val negativeInteger: Gen[Int] = negNum[Int]

  private def throwsUponCall[A](f: Function0[A]): Boolean = {
    try {
      f.apply
      false
    }
    catch {
      case _: IllegalArgumentException => true
      case _                          => false
    }
  }

  property("shallThrowWithNegNumInCaseClassApply") =
    forAll(negativeInteger) { i: Int => throwsUponCall[PositiveInt](() => PositiveInt(i)) }

  property("shallThrowWithNegNumInCaseClassApply") =
    forAll(positiveInteger) {
      i: Int => PositiveInt.build(i) == PositiveInt(i)
    }

}
