package spire.algebra

import spire.implicits._
import spire.math.Eq

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop._

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Laws[A] {

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

    property("quot") = forAll((x: A, y: A) =>
      y =!= A.zero ==> (((x * y) /~ y) === x)
    )
    property("mod = zero") = forAll((x: A, y: A) =>
      y =!= A.zero ==> (((x * y) % y) === A.zero)
    )
  }

  def euclideanRing(implicit A: EuclideanRing[A]) = _euclideanRing("euclideanRing", ring)

  def field(implicit A: Field[A]) = _euclideanRing("field", additiveAbGroup, multiplicativeAbGroup)

}

object Laws {

  def apply[A : Eq : Arbitrary] = new Laws[A] {
    def Eq = implicitly[Eq[A]]
    def Arbitrary = implicitly[Arbitrary[A]]
  }

}

trait LawChecker extends FunSuite with Checkers {

  def checkAll[A](name: String, props: Properties) {
    for ((id, prop) <- props.properties) {
      test(name + "." + id) {
        check(prop)
      }
    }
  }

}

// vim: expandtab:ts=2:sw=2
