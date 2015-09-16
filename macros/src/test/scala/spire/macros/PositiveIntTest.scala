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

  private def throwsUponCall[A, E](f: Function0[A]): Boolean = {
    try {
      f.apply
      false
    }
    catch {
      case _: E => true
      case _    => false
    }
  }

  val shallThrowWithNegNumInCaseClassApply: Prop =
    forAll(negativeInteger) { i: Int => throwsUponCall( () =>  PositiveInt(i) ) }

  val shallNotCompileWithNegConstNumInBuildMethod: Prop =
    forAll(negativeInteger) {
      i: Int => s"import spire.macros.PositiveInt; PositiveInt.build(${i}})".shouldNot(compile)
    }

  val shallReturnPositiveIntForPositiveIntConst: Prop =
    forAll(positiveInteger) {
      i: Int => PositiveInt.build(i) === PositiveInt(i)
    }

  shallThrowWithNegNumInCaseClassApply.check
  shallNotCompileWithNegConstNumInBuildMethod.check
  shallReturnPositiveIntForPositiveIntConst.check

  test("Constructing a `PositiveInt` via its case class with a (0 :Int) should throw") {
    assert( throwsUponCall( () => PositiveInt(0) ) )
  }

}
