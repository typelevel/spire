package spire.laws

import spire.algebra.{Eq, BooleanAlgebra, Heyting}
import spire.syntax.eq._
import spire.syntax.booleanAlgebra._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object LogicLaws {
  def apply[A: Eq: Arbitrary] = new LogicLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LogicLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def heytingAlgebra(implicit A: Heyting[A]) =
    new DefaultRuleSet(
      name = "heytingAlgebra",
      parent = None,
      "associative" -> forAll { (x: A, y: A, z: A) =>
        ((x & y) & z) === (x & (y & z)) && ((x | y) | z) === (x | (y | z))
      },

      "commutative" -> forAll { (x: A, y: A) =>
        (x & y) === (y & x) && (x | y) === (y | x)
      },

      "absorption" -> forAll { (x: A, y: A) =>
        (x & (x | y)) === x && (x | (x & y)) === x
      },

      "identity" -> forAll { (x: A) =>
        (x & A.one) === x && (x | A.zero) === x
      },

      "distributive" -> forAll { (x: A, y: A, z: A) =>
        (x & (y | z)) === ((x & y) | (x & z)) && (x | (y & z)) === ((x | y) & (x | z))
      },

      "consistent" -> forAll { (x: A) => (x & ~x) === A.zero },

      "¬x = (x → 0)" -> forAll { (x: A) => ~x === (x imp A.zero) },
      
      "x → x = 1" -> forAll { (x: A) => (x imp x) === A.one },
      
      "if x → y and y → x then x=y" -> forAll { (x: A, y: A) =>
        ((x imp y) =!= A.one) || ((y imp x) =!= A.one) || x === y
      },
      
      "if (1 → x)=1 then x=1" -> forAll { (x: A) =>
        ((A.one imp x) =!= A.one) || (x === A.one)
      },
      
      "x → (y → x) = 1" -> forAll { (x: A, y: A) => (x imp (y imp x)) === A.one },
      
      "(x→(y→z)) → ((x→y)→(x→z)) = 1" -> forAll { (x: A, y: A, z: A) =>
        ((x imp (y imp z)) imp ((x imp y) imp (x imp z))) === A.one
      },
      
      "x∧y → x = 1" -> forAll { (x: A, y: A) => ((x & y) imp x) === A.one },
      "x∧y → y = 1" -> forAll { (x: A, y: A) => ((x & y) imp y) === A.one },
      "x → y → (x∧y) = 1" -> forAll { (x: A, y: A) => (x imp (y imp (x & y))) === A.one },
      
      "x → x∨y" -> forAll { (x: A, y: A) => (x imp (x | y)) === A.one },
      "y → x∨y" -> forAll { (x: A, y: A) => (y imp (x | y)) === A.one },
      
      "(x → z) → ((y → z) → ((x | y) → z)) = 1" -> forAll { (x: A, y: A, z: A) =>
        ((x imp z) imp ((y imp z) imp ((x | y) imp z))) === A.one
      },
      
      "(0 → x) = 1" -> forAll { (x: A) => (A.zero imp x) === A.one }
    )

  def booleanAlgebra(implicit A: BooleanAlgebra[A]) =
    new DefaultRuleSet(
      name = "booleanAlgebra",
      parent = Some(heytingAlgebra),
      "excluded middle" -> forAll { (x: A) => (x | ~x) === A.one })
}
