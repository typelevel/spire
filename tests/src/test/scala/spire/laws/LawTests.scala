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
import spire.laws.discipline._
import spire.implicits.{ArrayEq => _, ArrayOrder => _, MapEq => _, MapGroup => _, SeqEq => _, SeqOrder => _, _}
import spire.laws.shadows.{Shadow, Shadowing}

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

  implicit val shadowingUByte: Shadowing[UByte, BigInt] = Shadowing.bigInt[UByte](s => UByte(s.toInt))
  implicit val shadowingUShort: Shadowing[UShort, BigInt] = Shadowing.bigInt[UShort](s => UShort(s.toInt))
  implicit val shadowingUInt: Shadowing[UInt, BigInt] = Shadowing.bigInt[UInt](s => UInt(s.toLong))
  implicit val shadowingULong: Shadowing[ULong, BigInt] = Shadowing.bigInt[ULong](s => ULong.fromBigInt(s))

  implicit val shadowingByte: Shadowing[Byte, BigInt] = Shadowing.bigInt[Byte](_.toByte)
  implicit val shadowingShort: Shadowing[Short, BigInt] = Shadowing.bigInt[Short](_.toShort)
  implicit val shadowingInt: Shadowing[Int, BigInt] = Shadowing.bigInt[Int](_.toInt)
  implicit val shadowingLong: Shadowing[Long, BigInt] = Shadowing.bigInt[Long](_.toLong)

  // Float and Double fail these tests
  checkAll("Byte",       LimitedRangeLaws[Byte].cRing)
  checkAll("Byte",       LimitedRangeLaws[Byte].signedGCDRing)
  checkAll("Short",      LimitedRangeLaws[Short].cRing)
  checkAll("Short",      LimitedRangeLaws[Short].signedGCDRing)
  checkAll("Int",        LimitedRangeLaws[Int].euclideanRing)
  checkAll("Int",        LimitedRangeLaws[Int].signedGCDRing)
  checkAll("Long",       LimitedRangeLaws[Long].euclideanRing)
  checkAll("Long",       LimitedRangeLaws[Long].signedGCDRing)
  checkAll("BigInt",     EuclideanRingTests[BigInt].euclideanRing)
  checkAll("BigInt",     SignedGCDRingTests[BigInt].signedGCDRing)
  checkAll("BigInteger", EuclideanRingTests[BigInteger].euclideanRing)
  checkAll("BigInteger", SignedGCDRingTests[BigInteger].signedGCDRing)
  checkAll("Rational",   FieldTests[Rational].field)
  checkAll("Rational",   SignedGCDRingTests[BigInt].signedGCDRing)
  checkAll("Real",       FieldTests[Real].field)
  checkAll("UByte",      CRigTests[UByte].cRig)
  checkAll("UByte",      SignedAdditiveCMonoidTests[Shadow[UByte, BigInt]].signedAdditiveCMonoid)
  checkAll("UShort",     CRigTests[UShort].cRig)
  checkAll("UShort",     SignedAdditiveCMonoidTests[Shadow[UShort, BigInt]].signedAdditiveCMonoid)
  checkAll("UInt",       CRigTests[UInt].cRig)
  checkAll("UInt",       SignedAdditiveCMonoidTests[Shadow[UInt, BigInt]].signedAdditiveCMonoid)
  checkAll("ULong",      CRigTests[ULong].cRig)
  checkAll("ULong",      SignedAdditiveCMonoidTests[Shadow[ULong, BigInt]].signedAdditiveCMonoid)
  checkAll("Natural",    CRigTests[Natural].cRig)
  checkAll("Natural",    SignedAdditiveCMonoidTests[Natural].signedAdditiveCMonoid)
  checkAll("SafeLong",   EuclideanRingTests[SafeLong].euclideanRing)
  checkAll("SafeLong",   SignedGCDRingTests[SafeLong].signedGCDRing)

  checkAll("Complex[Rational]", FieldTests[Complex[Rational]].field)

  checkAll("Quaternion[Rational]", DivisionRingTests[Quaternion[Rational]].divisionRing)

  checkAll("Levenshtein distance", BaseLaws[String].metricSpace)
  checkAll("BigInt",               BaseLaws[BigInt].metricSpace)

  // We skip checking all tuple types, as they are all generated from the same
  // template.
  checkAll("(Int,Int)",           RingTests[(Int, Int)].ring)
  checkAll("(Rational,Rational)", RingTests[(Rational, Rational)].ring)

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

  checkAll("List[Int]",   MonoidTests[List[Int]].monoid)
  checkAll("Vector[Int]", MonoidTests[Vector[Int]].monoid)
  checkAll("Set[Int]",    MonoidTests[Set[Int]].monoid(implicitly, Eq.fromUniversalEquals))
  checkAll("String[Int]", MonoidTests[String].monoid)
  checkAll("Array[Int]",  MonoidTests[Array[Int]].monoid)

  checkAll("Seq[String]", PartialGroupLaws[Seq[String]](Eq.fromUniversalEquals, implicitly).semigroupoid)
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

  checkAll("FreeMonoid", MonoidTests[FreeMonoid[String]].monoid)
  checkAll("D3", GroupTests[D3].group)
  checkAll("FreeGroup", GroupTests[FreeGroup[D3]].group)

  implicit def intAbGroup: AbGroup[Int] = AdditiveAbGroup[Int].additive
  checkAll("FreeAbGroup", AbGroupTests[FreeAbGroup[Int]].abGroup)

  checkAll("Bool[Boolean]", LogicLaws[Boolean].bool)
  checkAll("Bool[Int]", LogicLaws[Int].bool)
  checkAll("Heyting[Trilean]", LogicLaws[Int].heyting)

  object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int] with spire.std.IntOrder {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }

  checkAll("Order[Int]", OrderTests[Int].order)
  checkAll("Order[BigInteger]", OrderTests[BigInteger].order)
  checkAll("Order[Unit]", OrderTests[Unit].order)
  checkAll("AbGroup[Unit]", AbGroupTests[Unit].abGroup)
  checkAll("LatticePartialOrder[Int]", LatticePartialOrderLaws[Int].boundedLatticePartialOrder(intMinMaxLattice, implicitly[Order[Int]]))

  checkAll("Map[Int, Int]", PartialActionLaws.apply[Map[Int, Int], Seq[Int]](implicitly, Arbitrary(arbitrary[Perm].map(_.map)), implicitly, implicitly).groupPartialAction)
}
