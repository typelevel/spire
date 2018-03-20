package spire
package laws

import java.math.BigInteger
import spire.math.extras.{FixedPoint, FixedScale}

import spire.algebra._
import spire.algebra.free._
import spire.math._
import spire.math.interval.Bound

import org.scalacheck.Arbitrary

object arb {

  implicit val ubyte: Arbitrary[UByte] =
    Arbitrary(gen.ubyte)

  implicit val ushort: Arbitrary[UShort] =
    Arbitrary(gen.ushort)

  implicit val uint: Arbitrary[UInt] =
    Arbitrary(gen.uint)

  implicit val ulong: Arbitrary[ULong] =
    Arbitrary(gen.ulong)

  implicit val trilean: Arbitrary[Trilean] =
    Arbitrary(gen.trilean)

  implicit val fixedScale: Arbitrary[FixedScale] =
    Arbitrary(gen.fixedScale)

  implicit val fixedPoint: Arbitrary[FixedPoint] =
    Arbitrary(gen.fixedPoint)

  implicit val bigInteger: Arbitrary[BigInteger] =
    Arbitrary(gen.bigInteger)

  implicit val safeLong: Arbitrary[SafeLong] =
    Arbitrary(gen.safeLong)

  implicit val natural: Arbitrary[Natural] =
    Arbitrary(gen.natural)

  implicit val rational: Arbitrary[Rational] =
    Arbitrary(gen.rational)

  implicit val number: Arbitrary[Number] =
    Arbitrary(gen.number)

  implicit val algebraic: Arbitrary[Algebraic] =
    Arbitrary(gen.algebraic)

  implicit val real: Arbitrary[Real] =
    Arbitrary(gen.real)

  implicit val sign: Arbitrary[Sign] =
    Arbitrary(gen.sign)

  implicit def term[A: Arbitrary]: Arbitrary[poly.Term[A]] =
    Arbitrary(gen.term[A])

  implicit def polynomial[A: Arbitrary: Semiring: Eq: ClassTag]: Arbitrary[Polynomial[A]] =
    Arbitrary(gen.polynomial[A])

  implicit def complex[A: Arbitrary]: Arbitrary[Complex[A]] =
    Arbitrary(gen.complex[A])

  implicit def jet[A: Arbitrary: ClassTag]: Arbitrary[Jet[A]] =
    Arbitrary(gen.jet2[A])

  implicit def quaternion[A: Arbitrary]: Arbitrary[Quaternion[A]] =
    Arbitrary(gen.quaternion[A])

  implicit def bound[A: Arbitrary]: Arbitrary[Bound[A]] =
    Arbitrary(gen.bound[A])

  implicit def interval[A: Arbitrary: Order]: Arbitrary[Interval[A]] =
    Arbitrary(gen.interval[A])

  implicit def freeMonoid[A: Arbitrary]: Arbitrary[FreeMonoid[A]] =
    Arbitrary(gen.freeMonoid[A])

  implicit def freeGroup[A: Arbitrary]: Arbitrary[FreeGroup[A]] =
    Arbitrary(gen.freeGroup[A])

  implicit def freeAbGroup[A: Arbitrary]: Arbitrary[FreeAbGroup[A]] =
    Arbitrary(gen.freeAbGroup[A])

  implicit def perm: Arbitrary[Perm] =
    Arbitrary(gen.perm)
}
