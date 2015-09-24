package spire.macros

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll

class PositiveIntPropertyTest extends FunSuite with PropertyChecks with Matchers {

  test("throws exception on negative integers") {
    forAll(negNum[Int]) { (i: Int) =>
      an [IllegalArgumentException] should be thrownBy { PositiveInt(i) }
    }
  }

  test("successfully built with positive integers ") {
    forAll(posNum[Int]) { (i: Int) =>
      PositiveInt(i).value shouldBe i
    }
  }
}
