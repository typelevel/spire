package spire.algebra

import spire.implicits.{
  SeqOrder => _, SeqEq => _,
  ArrayOrder => _, ArrayEq => _,
  MapEq => _,
  _ }
import spire.math._

import scala.{ specialized => spec }

class LawTests extends LawChecker {

  import SpireArbitrary._

  def fuzzyEq[@spec(Float,Double) A: Ring: Signed: Order](eps: A): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = {
      val delta = Order[A].max(x.abs, y.abs) * eps
      println("d = %f, (x - y).abs = %f" format (delta, (x - y).abs))
      (x - y).abs < delta
    }
  }

  // Float and Double fail these tests
  checkAll("Int",      RingLaws[Int].euclideanRing)
  checkAll("Long",     RingLaws[Long].euclideanRing)
  checkAll("BigInt",   RingLaws[BigInt].euclideanRing)
  checkAll("Rational", RingLaws[Rational].field)
  checkAll("Real",     RingLaws[Real].field)

  checkAll("Levenshtein distance", BaseLaws[String].metricSpace)
  checkAll("BigInt",               BaseLaws[BigInt].metricSpace)

  // We skip checking all tuple types, as they are all generated from the same
  // template.
  checkAll("(Int,Int)",           RingLaws[(Int, Int)].ring)
  checkAll("(Rational,Rational)", RingLaws[(Rational, Rational)].withPred(Predicate { case (r1, r2) =>
    val P = implicitly[Predicate[Rational]]
    P(r1) && P(r2)
  }).field)

  import spire.optional.vectorOrder._

  // Testing all A <: Seq is redundant, as we treat them uniformly via.
  // iterators and CanBuildFroms. So, presuming the Scala std lib is tested,
  // testing just List and Vector should suffice for us.

  checkAll("List[Int]",        VectorSpaceLaws[List[Int], Int].module)
  checkAll("Vector[Int]",      VectorSpaceLaws[Vector[Int], Int].module)
  checkAll("List[Rational]",   VectorSpaceLaws[List[Rational], Rational].vectorSpace)
  checkAll("Vector[Rational]", VectorSpaceLaws[Vector[Rational], Rational].vectorSpace)
  
  checkAll("Array[Int]",         VectorSpaceLaws[Array[Int], Int].module)
  checkAll("Array[VectorSpace]", VectorSpaceLaws[Array[Rational], Rational].vectorSpace)

  checkAll("Map[String,Int]",      VectorSpaceLaws[Map[String,Int], Int].module)
  checkAll("Map[String,Rational]", VectorSpaceLaws[Map[String,Rational], Rational].vectorSpace)

  val max = NormedVectorSpace.max[Rational, List]
  checkAll("List[Rational]",
    VectorSpaceLaws[List[Rational], Rational].normedVectorSpace(max, implicitly, implicitly))

  checkAll("List[Int]",   GroupLaws[List[Int]].monoid)
  checkAll("Vector[Int]", GroupLaws[Vector[Int]].monoid)
  checkAll("Set[Int]",    GroupLaws[Set[Int]](spire.optional.genericEq.generic, implicitly).monoid)
  checkAll("String[Int]", GroupLaws[String].monoid)
  checkAll("Array[Int]",  GroupLaws[Array[Int]].monoid)

  checkAll("String", VectorSpaceLaws[String, Int].metricSpace)
}

// vim: expandtab:ts=2:sw=2
