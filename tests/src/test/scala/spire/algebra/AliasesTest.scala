package spire
package algebra

import org.scalatest.funsuite.AnyFunSuite

class AliasesTest extends AnyFunSuite {

  case class Cats(i: Int)

  object Cats {
    implicit object equ extends cats.kernel.Eq[Cats] {
      def eqv(x: Cats, y: Cats) = (x.i == y.i)
    }
  }

  case class Algebra(i: Int)

  object Algebra {
    implicit object equ extends _root_.algebra.Eq[Algebra] {
      def eqv(x: Algebra, y: Algebra) = (x.i == y.i)
    }
  }

  case class Spire(i: Int)

  object Spire {
    implicit object equ extends spire.algebra.Eq[Spire] {
      def eqv(x: Spire, y: Spire) = (x.i == y.i)
    }
  }

  test("Implicit resolution works through aliases") {
    assert(implicitly[cats.kernel.Eq[Cats]].eqv(Cats(1), Cats(1)))
    assert(implicitly[cats.kernel.Eq[Algebra]].eqv(Algebra(1), Algebra(1)))
    assert(implicitly[cats.kernel.Eq[Spire]].eqv(Spire(1), Spire(1)))

    assert(implicitly[_root_.algebra.Eq[Cats]].eqv(Cats(1), Cats(1)))
    assert(implicitly[_root_.algebra.Eq[Algebra]].eqv(Algebra(1), Algebra(1)))
    assert(implicitly[_root_.algebra.Eq[Spire]].eqv(Spire(1), Spire(1)))

    assert(implicitly[spire.algebra.Eq[Cats]].eqv(Cats(1), Cats(1)))
    assert(implicitly[spire.algebra.Eq[Algebra]].eqv(Algebra(1), Algebra(1)))
    assert(implicitly[spire.algebra.Eq[Spire]].eqv(Spire(1), Spire(1)))
  }

}
