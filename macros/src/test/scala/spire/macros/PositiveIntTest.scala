package spire.macros

import org.scalatest.Matchers

import org.scalatest._

import org.scalacheck.{Prop, Gen}
import Gen.{posNum, negNum}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._

/**
 * Verifies that this macro will only compile successfully for `Constant` AST
 * values that are > 0.
 */
class PositiveIntTest extends FunSuite with Matchers {

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

  val shallThrowWithNegNumInCaseClassApply: Prop =
    forAll(negativeInteger) { i: Int => throwsUponCall[PositiveInt]( () =>  PositiveInt(i) ) }

  val shallReturnPositiveIntForPositiveIntConst: Prop =
    forAll(positiveInteger) {
      i: Int => PositiveInt.build(i) === PositiveInt(i)
    }

  shallThrowWithNegNumInCaseClassApply.check
  shallReturnPositiveIntForPositiveIntConst.check

  test("Constructing with its companion method, 'build', should fail to compile with a negative numbers.") {
    "import spire.macros.PositiveInt; PositiveInt.build(-55)".shouldNot(compile)
    "import spire.macros.PositiveInt; PositiveInt.build(-1)".shouldNot(compile)
    "import spire.macros.PositiveInt; PositiveInt.build(-9999)".shouldNot(compile)
    "import spire.macros.PositiveInt; PositiveInt.build(-1234)".shouldNot(compile)
  }

  test("Constructing a `PositiveInt` via its case class with a (0 :Int) should throw") {
    assert( throwsUponCall[PositiveInt]( () => PositiveInt(0) ) )
  }

}
