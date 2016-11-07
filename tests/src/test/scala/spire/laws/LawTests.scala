package spire
package laws

import java.math.BigInteger
import spire.algebra._
import spire.algebra.free._
import spire.algebra.lattice._
import spire.laws.arb._
import spire.math._
import spire.optional.partialIterable._
import spire.optional.mapIntIntPermutation._

import spire.implicits.{
  SeqOrder => _, SeqEq => _,
  ArrayOrder => _, ArrayEq => _,
  MapEq => _, MapGroup => _,
  _ }

import org.typelevel.discipline.scalatest.Discipline

import org.scalatest.FunSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

class LawTests extends FunSuite with Discipline {

  def fuzzyEq[@sp(Float,Double) A: Ring: Signed: Order](eps: A): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = {
      val delta = Order[A].max(x.abs, y.abs) * eps
      println("d = %f, (x - y).abs = %f" format (delta, (x - y).abs))
      (x - y).abs < delta
    }
  }

  // Float and Double fail these tests
  checkAll("Int",        RingLaws[Int].euclideanRing)
  checkAll("Int",        OrderLaws[Int].truncatedDivisionLimitedRange)
  checkAll("Long",       RingLaws[Long].euclideanRing)
  checkAll("Int",        OrderLaws[Long].truncatedDivisionLimitedRange)
  checkAll("BigInt",     RingLaws[BigInt].euclideanRing)
  checkAll("BigInt",     OrderLaws[BigInt].truncatedDivision)
  checkAll("BigInteger", RingLaws[BigInteger].euclideanRing)
  checkAll("Rational",   RingLaws[Rational].field)
  checkAll("Rational",   OrderLaws[Rational].truncatedDivision)
  checkAll("Real",       RingLaws[Real].field)

  checkAll("Levenshtein distance", BaseLaws[String].metricSpace)
  checkAll("BigInt",               BaseLaws[BigInt].metricSpace)

  // We skip checking all tuple types, as they are all generated from the same
  // template.
  checkAll("(Int,Int)",           RingLaws[(Int, Int)].ring)
  checkAll("(Rational,Rational)", RingLaws[(Rational, Rational)].ring)

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

  checkAll("Seq[String]", PartialGroupLaws[Seq[String]](spire.optional.genericEq.generic, implicitly).semigroupoid)
  checkAll("Seq[Int]",    PartialGroupLaws[Seq[Int]].groupoid)

  checkAll("String", VectorSpaceLaws[String, Int].metricSpace)

  checkAll("Sign", ActionLaws[Sign, Int].multiplicativeMonoidAction)

  implicit def eqFreeMonoid[A: Monoid: Eq]: Eq[FreeMonoid[A]] = new Eq[FreeMonoid[A]] {
    def eqv(x: FreeMonoid[A], y: FreeMonoid[A]): Boolean =
      Eq[A].eqv(x.run(n => n), y.run(n => n))
  }

  implicit def eqFreeGroup[A: Group: Eq]: Eq[FreeGroup[A]] = new Eq[FreeGroup[A]] {
    def eqv(x: FreeGroup[A], y: FreeGroup[A]): Boolean =
      Eq[A].eqv(x.run(n => n), y.run(n => n))
  }

  implicit def eqFreeAbGroup[A: AbGroup: Eq]: Eq[FreeAbGroup[A]] = new Eq[FreeAbGroup[A]] {
    def eqv(x: FreeAbGroup[A], y: FreeAbGroup[A]): Boolean =
      Eq[A].eqv(x.run(n => n), y.run(n => n))
  }

  checkAll("FreeMonoid", GroupLaws[FreeMonoid[String]].monoid)
  checkAll("D3", GroupLaws[D3].group)
  checkAll("FreeGroup", GroupLaws[FreeGroup[D3]].group)

  implicit def intAbGroup: AbGroup[Int] = AdditiveAbGroup[Int].additive
  checkAll("FreeAbGroup", GroupLaws[FreeAbGroup[Int]].abGroup)

  checkAll("Bool[Boolean]", LogicLaws[Boolean].bool)
  checkAll("Bool[Int]", LogicLaws[Int].bool)
  checkAll("Heyting[Trilean]", LogicLaws[Int].heyting)

  object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int] with spire.std.IntOrder {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }

  checkAll("Order[Int]", OrderLaws[Int].order)
  checkAll("Order[BigInteger]", OrderLaws[BigInteger].order)
  checkAll("Order[Unit]", OrderLaws[Unit].order)
  checkAll("AbGroup[Unit]", GroupLaws[Unit].abGroup)
  checkAll("LatticePartialOrder[Int]", LatticePartialOrderLaws[Int].boundedLatticePartialOrder(intMinMaxLattice, implicitly[Order[Int]]))

  checkAll("Map[Int, Int]", PartialActionLaws.apply[Map[Int, Int], Seq[Int]](implicitly, Arbitrary(arbitrary[Perm].map(_.map)), implicitly, implicitly).groupPartialAction)
}
