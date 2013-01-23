package spire.algebra

import spire.implicits._
import spire.math._

import scala.{ specialized => spec }

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

class LawTests extends LawChecker {

  def fuzzyEq[@spec(Float,Double) A: Ring: Signed: Order](eps: A): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = {
      val delta = Order[A].max(x.abs, y.abs) * eps
      println("d = %f, (x - y).abs = %f" format (delta, (x - y).abs))
      (x - y).abs < delta
    }
  }

  implicit def RationalArbitrary: Arbitrary[Rational] = Arbitrary(arbitrary[Double] map (Rational(_)))
  implicit def RealArbitrary: Arbitrary[Real] = Arbitrary(arbitrary[Int] map (Real(_)))
  implicit def VectorArbitrary[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(arbitrary[List[A]] map (Vector(_: _*)))

  checkAll("Int", Laws[Int].euclideanRing)
  checkAll("Long", Laws[Long].euclideanRing)
  // checkAll("Float", Laws[Float].field)
  // checkAll("Double", Laws[Double].field)
  checkAll("BigInt", Laws[BigInt].euclideanRing)
  checkAll("Rational", Laws[Rational].field)
  checkAll("Real", Laws[Real].field)

  // We skip checking all tuple types, as they are all generated from the same
  // template.
  checkAll("Tuple2 is Ring", Laws[(Int, Int)].ring)
  checkAll("Tuple2 is Field", Laws[(Rational, Rational)].field)

  import spire.optional.vectorOrder._

  // Testing all A <: Seq is redundant, as we treat them uniformly via.
  // iterators and CanBuildFroms. So, presuming the Scala std lib is tested,
  // testing just List and Vector should suffice for us.

  checkAll("List is Module", VectorSpaceLaws[List[Int], Int].module)
  checkAll("Vector is Module", VectorSpaceLaws[Vector[Int], Int].module)
  checkAll("List is VectorSpace", VectorSpaceLaws[List[Rational], Rational].vectorSpace)
  checkAll("Vector is VectorSpcae", VectorSpaceLaws[Vector[Rational], Rational].vectorSpace)
  
  checkAll("Array is Module", VectorSpaceLaws[Array[Int], Int].module)
  checkAll("Array is VectorSpace", VectorSpaceLaws[Array[Rational], Rational].vectorSpace)

  checkAll("Map is Module", VectorSpaceLaws[Map[String,Int], Int].module)
  checkAll("Map is VectorSpace", VectorSpaceLaws[Map[String,Rational], Rational].vectorSpace)

  val max = NormedVectorSpace.max[Rational, List]
  checkAll("L_inf NormedVectorSpace",
    VectorSpaceLaws[List[Rational], Rational].normedVectorSpace(max, implicitly, implicitly))

  checkAll("List is a Monoid", Laws[List[Int]].monoid)
  checkAll("Vector is a Monoid", Laws[Vector[Int]].monoid)
  checkAll("Set is a Monoid", Laws[Set[Int]](spire.optional.genericEq.generic, implicitly).monoid)
  checkAll("String is a Monoid", Laws[String].monoid)
}

// vim: expandtab:ts=2:sw=2
