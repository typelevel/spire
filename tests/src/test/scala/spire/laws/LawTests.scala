package spire
package laws

import java.math.BigInteger
import spire.algebra._
import spire.algebra.free._
import spire.algebra.lattice._
import spire.laws.arb._
import spire.laws.shadows.{Shadow, Shadowing}
import spire.math._
import spire.optional.Perm
import spire.optional.partialIterable._

import spire.implicits.{
  SeqOrder => _, SeqEq => _,
  ArrayOrder => _, ArrayEq => _,
  MapEq => _, MapGroup => _,
  _ }

import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

class LawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  def fuzzyEq[@sp(Float,Double) A: Ring: Signed: Order](eps: A): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = {
      val delta = Order[A].max(x.abs, y.abs) * eps
      println("d = %f, (x - y).abs = %f" format (delta, (x - y).abs))
      (x - y).abs < delta
    }
  }

  // shadowed types with limited range

  implicit val shadowingUByte: Shadowing[UByte, BigInt] = Shadowing.bigInt[UByte](s => UByte(s.toInt))
  implicit val shadowingUShort: Shadowing[UShort, BigInt] = Shadowing.bigInt[UShort](s => UShort(s.toInt))
  implicit val shadowingUInt: Shadowing[UInt, BigInt] = Shadowing.bigInt[UInt](s => UInt(s.toLong))
  implicit val shadowingULong: Shadowing[ULong, BigInt] = Shadowing.bigInt[ULong](s => ULong.fromBigInt(s))

  implicit val shadowingByte: Shadowing[Byte, BigInt] = Shadowing.bigInt[Byte](s => s.toByte)
  implicit val shadowingShort: Shadowing[Short, BigInt] = Shadowing.bigInt[Short](s => s.toShort)
  implicit val shadowingInt: Shadowing[Int, BigInt] = Shadowing.bigInt[Int](s => s.toInt)
  implicit val shadowingLong: Shadowing[Long, BigInt] = Shadowing.bigInt[Long](s => s.toLong)

  checkAll("UByte",      RingLaws[UByte].cRig)
  checkAll("UByte",      OrderLaws[Shadow[UByte, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("UByte",      CombinationLaws[Shadow[UByte, BigInt]].signedAdditiveCMonoid)

  checkAll("UShort",     RingLaws[UShort].cRig)
  checkAll("UShort",     OrderLaws[Shadow[UShort, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("UShort",     CombinationLaws[Shadow[UShort, BigInt]].signedAdditiveCMonoid)

  checkAll("UInt",       RingLaws[UInt].cRig)
  checkAll("UInt",       OrderLaws[Shadow[UInt, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("UInt",       CombinationLaws[Shadow[UInt, BigInt]].signedAdditiveCMonoid)

  checkAll("ULong",      RingLaws[ULong].cRig)
  checkAll("ULong",      OrderLaws[Shadow[ULong, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("ULong",      CombinationLaws[Shadow[ULong, BigInt]].signedAdditiveCMonoid)

  checkAll("Natural",    RingLaws[Natural].cRig)
  checkAll("Natural",    CombinationLaws[Natural].signedAdditiveCMonoid)
  checkAll("Natural",    OrderLaws[Natural].truncatedDivision)

  // Float and Double fail these tests
  checkAll("Byte",       RingLaws[Byte].cRing)
  checkAll("Byte",       RingLaws[Shadow[Byte, BigInt]].integerEuclideanRing(Shadow.euclideanRing, Shadow.signed))
  checkAll("Byte",       CombinationLaws[Shadow[Byte, BigInt]].signedAdditiveCMonoid)
  checkAll("Byte",       OrderLaws[Shadow[Byte, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("Byte",       BaseLaws[Byte].uniqueFactorizationDomain)

  checkAll("Short",      RingLaws[Short].cRing)
  checkAll("Short",      RingLaws[Shadow[Short, BigInt]].integerEuclideanRing(Shadow.euclideanRing, Shadow.signed))
  checkAll("Short",      CombinationLaws[Shadow[Short, BigInt]].signedAdditiveCMonoid)
  checkAll("Short",      OrderLaws[Shadow[Short, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("Short",      BaseLaws[Short].uniqueFactorizationDomain)
  
  checkAll("Int",        RingLaws[Int].cRing)
  checkAll("Int",        RingLaws[Shadow[Int, BigInt]].integerEuclideanRing(Shadow.euclideanRing, Shadow.signed))
  checkAll("Int",        BaseLaws[Int].uniqueFactorizationDomain)
  checkAll("Int",        OrderLaws[Shadow[Int, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("Int",        CombinationLaws[Shadow[Int, BigInt]].signedAdditiveCMonoid)

  checkAll("Long",       RingLaws[Long].cRing)
  checkAll("Long",       RingLaws[Shadow[Long, BigInt]].integerEuclideanRing(Shadow.euclideanRing, Shadow.signed))
  checkAll("Long",       BaseLaws[Long].uniqueFactorizationDomain)
  checkAll("Long",       OrderLaws[Shadow[Long, BigInt]].truncatedDivision(Shadow.cRig, Shadow.truncatedDivision))
  checkAll("Long",       CombinationLaws[Shadow[Long, BigInt]].signedAdditiveCMonoid)

  // to test ShadowInvolution
  checkAll("Long",       InvolutionLaws[Shadow[Long, BigInt]].involutionRing(Shadow.involution, Shadow.cRing))

  checkAll("BigInt",     RingLaws[BigInt].integerEuclideanRing)
  checkAll("BigInt",     CombinationLaws[BigInt].signedGCDRing)
  checkAll("BigInt",     OrderLaws[BigInt].truncatedDivision)
  checkAll("BigInt",     BaseLaws[BigInt].metricSpace)
  // checkAll("BigInt",     BaseLaws[BigInt].uniqueFactorizationDomain) // TODO: fast enough

  checkAll("BigInteger", RingLaws[BigInteger].integerEuclideanRing)
  checkAll("BigInteger", CombinationLaws[BigInteger].signedGCDRing)
  checkAll("BigInteger", OrderLaws[BigInteger].truncatedDivision)

  checkAll("Rational",   RingLaws[Rational].field)
  checkAll("Rational",   CombinationLaws[Rational].signedGCDRing)
  checkAll("Rational",   OrderLaws[Rational].truncatedDivision)
  checkAll("Rational",   InvolutionLaws[Rational].involutionRing)

  checkAll("Real",       RingLaws[Real].field)

  checkAll("SafeLong",   RingLaws[SafeLong].integerEuclideanRing)
  checkAll("SafeLong",   CombinationLaws[SafeLong].signedGCDRing)
  checkAll("SafeLong",   OrderLaws[SafeLong].truncatedDivision)
  // checkAll("SafeLong",   BaseLaws[SafeLong].uniqueFactorizationDomain) // TODO: fast enough?

  checkAll("Order[Unit]",OrderLaws[Unit].order)

  // complex
  checkAll("Complex[Rational]",   RingLaws[Complex[Rational]].field)
  checkAll("Complex[Rational]",   InvolutionLaws[Complex[Rational]].involutionAlgebra[Rational])
  checkAll("Complex[SafeLong]",   RingLaws[Complex[SafeLong]].cRing)
  checkAll("Complex[SafeLong]",   InvolutionLaws[Complex[SafeLong]].involutionRing)

  checkAll("Quaternion[Rational]", RingLaws[Quaternion[Rational]].divisionRing)
  checkAll("Quaternion[Rational]", InvolutionLaws[Quaternion[Rational]].involutionAlgebra[Rational])

  checkAll("Levenshtein distance", BaseLaws[String].metricSpace)

  // We skip checking all tuple types, as they are all generated from the same
  // template.
  checkAll("(Int,Int)",           RingLaws[(Int, Int)].ring)
  checkAll("(Rational,Rational)", RingLaws[(Rational, Rational)].ring)

  import spire.optional.vectorOrder._

  // Testing all A <: Seq is redundant, as we treat them uniformly via.
  // iterators and CanBuildFroms. So, presuming the Scala std lib is tested,
  // testing just List and Vector should suffice for us.

  checkAll("List[Int]",        VectorSpaceLaws[List[Int], Int].cModule)
  checkAll("Vector[Int]",      VectorSpaceLaws[Vector[Int], Int].cModule)
  checkAll("List[Rational]",   VectorSpaceLaws[List[Rational], Rational].vectorSpace)
  checkAll("Vector[Rational]", VectorSpaceLaws[Vector[Rational], Rational].vectorSpace)

  checkAll("Array[Int]",         VectorSpaceLaws[Array[Int], Int].cModule)
  checkAll("Array[VectorSpace]", VectorSpaceLaws[Array[Rational], Rational].vectorSpace)

  checkAll("Map[String,Int]",      VectorSpaceLaws[Map[String,Int], Int].cModule)
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
  checkAll("DeMorgan[Trilean]", LogicLaws[Trilean].deMorgan)

  object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int] with spire.std.IntOrder {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }

  checkAll("AbGroup[Unit]", GroupLaws[Unit].abGroup)
  checkAll("LatticePartialOrder[Int]", LatticePartialOrderLaws[Int].boundedLatticePartialOrder(intMinMaxLattice, implicitly[Order[Int]]))

  checkAll("Perm", GroupLaws[Perm].group)
  checkAll("Perm", ActionLaws[Perm, Int].groupAction)
  checkAll("Perm", PartialActionLaws[Perm, Seq[Int]].groupPartialAction)
}
