package spire.algebra

import spire.implicits._

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

trait AlgebraLaws[A] {

  implicit def Eq: Eq[A]
  implicit def Arbitrary: Arbitrary[A]

  def semigroup(implicit A: Semigroup[A]) = new Properties("semigroup") {
    property("associative") = forAll((x: A, y: A, z: A) =>
      ((x |+| y) |+| z) === (x |+| (y |+| z))
    )
  }

  def monoid(implicit A: Monoid[A]) = new Properties("monoid") {
    include(semigroup)
    property("left identity")  = forAll((x: A) => (A.id |+| x) === x)
    property("right identity") = forAll((x: A) => (x |+| A.id) === x)
  }

  def group(implicit A: Group[A]) = new Properties("group") {
    include(monoid)
    property("left inverse")  = forAll((x: A) => A.id === (x.inverse |+| x))
    property("right inverse") = forAll((x: A) => A.id === (x |+| x.inverse))
  }

  def abGroup(implicit A: AbGroup[A]) = new Properties("abGroup") {
    include(group)
    property("commutative") = forAll((x: A, y: A) =>
      (x |+| y) === (y |+| x)
    )
  }

  def additiveSemigroup(implicit A: AdditiveSemigroup[A]) = new Properties("additive") {
    include(semigroup(A.additive))
  }

  def additiveMonoid(implicit A: AdditiveMonoid[A]) = new Properties("additive") {
    include(monoid(A.additive))
  }

  def additiveGroup(implicit A: AdditiveGroup[A]) = new Properties("additive") {
    include(group(A.additive))
  }

  def additiveAbGroup(implicit A: AdditiveAbGroup[A]) = new Properties("additive") {
    include(abGroup(A.additive))
  }

  def multiplicativeSemigroup(implicit A: MultiplicativeSemigroup[A]) = new Properties("multiplicative") {
    include(semigroup(A.multiplicative))
  }

  def multiplicativeMonoid(implicit A: MultiplicativeMonoid[A]) = new Properties("multiplicative") {
    include(monoid(A.multiplicative))
  }

  def multiplicativeGroup(implicit A: MultiplicativeGroup[A]) = new Properties("multiplicative") {
    include(group(A.multiplicative))
  }

  def multiplicativeAbGroup(implicit A: MultiplicativeAbGroup[A]) = new Properties("multiplicative") {
    include(abGroup(A.multiplicative))
  }

  def semiring(implicit A: Semiring[A]) = new Properties("semiring") {
    include(additiveSemigroup)
    include(multiplicativeSemigroup)
    property("distributive") = forAll { (x: A, y: A, z: A) =>
      (x * (y + z) === (x * y + x * z)) && (((x + y) * z) === (x * z + y * z))
    }
  }

  def rng(implicit A: Rng[A]) = new Properties("rng") {
    include(additiveAbGroup)
    include(multiplicativeSemigroup)
  }

  def rig(implicit A: Rig[A]) = new Properties("rig") {
    include(additiveMonoid)
    include(multiplicativeMonoid)
  }

  def ring(implicit A: Ring[A]) = new Properties("ring") {
    include(additiveAbGroup)
    include(multiplicativeMonoid)
  }

  private def _euclideanRing(name: String, includes: Properties*)(implicit A: EuclideanRing[A]) = new Properties(name) {
    includes foreach include

    // This isn't necessarily true: x = 1/4, y = 3, (x * y) /~ y === 0.
    // property("quot") = forAll((x: A, y: A) =>
    //   y =!= A.zero ==> (((x * y) /~ y) === x)
    // )
    //
    // This is not true, in general: 2 * (1 / 4) % 2 == 1 / 2. But, we should
    // be testing something here.
    // property("mod = zero") = forAll((x: A, y: A) =>
    //   y =!= A.zero ==> (((x * y) % y) === A.zero)
    // )
  }

  def euclideanRing(implicit A: EuclideanRing[A]) = _euclideanRing("euclideanRing", ring)

  def field(implicit A: Field[A]) = new Properties("field") {
    include(euclideanRing)
    include(multiplicativeAbGroup)
    // _euclideanRing("field", additiveAbGroup, multiplicativeAbGroup)
  }

  def signed(implicit A: Signed[A]) = new Properties("signed") {
    property("abs non-negative") = forAll((x: A) => x.abs != Negative)
  }
}

object AlgebraLaws {
  def apply[A : Eq : Arbitrary] = new AlgebraLaws[A] {
    def Eq = implicitly[Eq[A]]
    def Arbitrary = implicitly[Arbitrary[A]]
  }
}

// vim: expandtab:ts=2:sw=2
