package spire
package math

import org.typelevel.discipline.scalatest._

import org.scalatest._
import org.scalatest.prop._

import shapeless._

import spire.implicits._
import ArbitrarySupport._
// import spire.laws.GroupLaws
// import spire.implicits.{FloatAlgebra => _, _}

class VecTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks with Matchers {
  // implicit val fuzzyAlgebraFloat: spire.std.FloatAlgebra = new spire.std.FloatAlgebra {
  //   override def eqv(x: Float, y: Float): Boolean = {
  //     val percent = Math.abs((x / 20f))
  //     x === (y +- (Math.max(percent, 0.01f)))
  //   }
  // }

  // implicit val arbFloat: Arbitrary[Float] = Arbitrary {
  //   Gen.choose(-100f, 100f)
  // }

  test("Vec[3, 3].cross.dot === 0") {
    forAll { (v1: Vec3f, v2: Vec3f) =>
      assert(((v1.normalize × v2.normalize) ⋅ v1.normalize) === (0f +- 0.1f))
    }
  }
  test("Vec[3, 3].cross is distributive") {
    forAll { (v1: Vec3f, v2: Vec3f, v3: Vec3f) =>
      assert(v1 × (v2 + v3) === ((v1 × v2) + (v1 × v3)))
    }
  }

  test("Vec basis[2, 3] is the z axis in 3D") {
    Vec.basis[nat._2, nat._3, Int] should ===(Vec.sized[nat._3, Int](Vector(0, 0, 1)))
  }

  test("cross product follows right hand rule") {
    (Vec.sized[nat._3, Float](Vector(1f, 0f, 0f)) × Vec.sized[nat._3, Float](Vector(0f, 1f, 0f))) should ===(Vec.sized[nat._3, Float](Vector(0f, 0f, 1f)))
  }

  test("padOne pads a vector with 1") {
    Vec.sized[nat._2, Int](Vector(0, 0)).padOne(3) should ===(Vec.sized[nat._3, Int](Vector(0, 0, 1)))
  }
}
