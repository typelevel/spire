package spire.laws.shadows

import spire.algebra._
import org.scalacheck.Arbitrary

/** Represents a primitive value `a: A` along with its shadow `s: S`.
  *
  * The shadow is a type S isomorphic to the primitive type A
  * in the range where A is defined.
  */
case class Shadow[A, S](a: A, s: S)

abstract class ShadowInstances0 {
  
  implicit def additiveCSemigroup[A:AdditiveCSemigroup,S:AdditiveCSemigroup](implicit ev: Shadowing[A, S]): AdditiveCSemigroup[Shadow[A, S]] =
    new ShadowAdditiveCSemigroup[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
    }

  implicit def multiplicativeCSemigroup[A:MultiplicativeCSemigroup,S:MultiplicativeCSemigroup](implicit ev: Shadowing[A, S]): MultiplicativeCSemigroup[Shadow[A, S]] =
    new ShadowMultiplicativeCSemigroup[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
    }

  implicit def eqInstance[A:Eq, S:Eq]: Eq[Shadow[A, S]] =
    new ShadowEq[A, S] {
      def A = implicitly
      def S = implicitly
    }

  implicit def conjugation[A:Conjugation, S:Conjugation](implicit ev: Shadowing[A, S]): Conjugation[Shadow[A, S]] =
    new ShadowConjugation[A, S] {
      def A: Conjugation[A] = implicitly
      def S: Conjugation[S] = implicitly
      val shadowing: Shadowing[A, S] = ev
    }
}


abstract class ShadowInstances1 extends ShadowInstances0 {

  implicit def additiveCMonoid[A:Eq:AdditiveCMonoid,S:Eq:AdditiveCMonoid](implicit ev: Shadowing[A, S]): AdditiveCMonoid[Shadow[A, S]] =
    new ShadowAdditiveCMonoid[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }

  implicit def multiplicativeCMonoid[A:Eq:MultiplicativeCMonoid,S:Eq:MultiplicativeCMonoid](implicit ev: Shadowing[A, S]): MultiplicativeCMonoid[Shadow[A, S]] =
    new ShadowMultiplicativeCMonoid[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }

  implicit def partialOrderInstance[A:PartialOrder, S:PartialOrder]: PartialOrder[Shadow[A, S]] =
    new ShadowPartialOrder[A, S] {
      def A = implicitly
      def S = implicitly
    }
}

abstract class ShadowInstances2 extends ShadowInstances1 {

  def additiveAbGroup[A:Eq:AdditiveAbGroup,S:Eq:AdditiveAbGroup](implicit ev: Shadowing[A, S]): AdditiveAbGroup[Shadow[A, S]] =
    new ShadowAdditiveAbGroup[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }

  implicit def orderInstance[A:Order, S:Order]: Order[Shadow[A, S]] =
    new ShadowOrder[A, S] {
      def A = implicitly
      def S = implicitly
    }
}

abstract class ShadowInstances3 extends ShadowInstances2 {

  def cSemiring[A:Eq:CSemiring,S:Eq:CSemiring](implicit ev: Shadowing[A, S]): CSemiring[Shadow[A, S]] =
    new ShadowCSemiring[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }
  
  implicit def signedInstance[A:Signed, S:Signed](implicit ev: Shadowing[A, S]): Signed[Shadow[A, S]] =
    new ShadowSigned[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
    }
}

abstract class ShadowInstances4 extends ShadowInstances3 {

  def cRig[A:Eq:CRig,S:Eq:CRig](implicit ev: Shadowing[A, S]): CRig[Shadow[A, S]] =
    new ShadowCRig[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }

  def cRng[A:Eq:CRng,S:Eq:CRng](implicit ev: Shadowing[A, S]): CRng[Shadow[A, S]] =
    new ShadowCRng[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }

  implicit def truncatedDivision[A:TruncatedDivision, S:TruncatedDivision](implicit ev: Shadowing[A, S]): TruncatedDivision[Shadow[A, S]] =
    new ShadowTruncatedDivision[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
    }

}

abstract class ShadowInstances5 extends ShadowInstances4 {

  def cRing[A:Eq:CRing,S:Eq:CRing](implicit ev: Shadowing[A, S]): CRing[Shadow[A, S]] =
    new ShadowCRing[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }
}

abstract class ShadowInstances6 extends ShadowInstances5 {

  def gcdRing[A:Eq:GCDRing,S:Eq:GCDRing](implicit ev: Shadowing[A, S]): GCDRing[Shadow[A, S]] =
    new ShadowGCDRing[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }
}

abstract class ShadowInstances7 extends ShadowInstances6 {
  
  def euclideanRing[A:Eq:EuclideanRing,S:Eq:EuclideanRing](implicit ev: Shadowing[A, S]): EuclideanRing[Shadow[A, S]] =
    new ShadowEuclideanRing[A, S] {
      val shadowing = ev
      def A = implicitly
      def S = implicitly
      def eqA = implicitly
      def eqS = implicitly
    }
}

object Shadow extends ShadowInstances7 {
  implicit def spireLawsArbitraryShadow[A, S](implicit A: Arbitrary[A], S: Shadowing[A, S]): Arbitrary[Shadow[A, S]] =
    Arbitrary { A.arbitrary.map( a => Shadow(a, S.toShadow(a)) ) }
}
