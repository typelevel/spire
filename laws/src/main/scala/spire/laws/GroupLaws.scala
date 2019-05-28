package spire
package laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import InvalidTestException._

object GroupLaws {
  def apply[A : Eq : Arbitrary] = new GroupLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait GroupLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  // groups

  def semigroup(implicit A: Semigroup[A]) = new GroupProperties(
    name = "semigroup",
    parent = None,
    "associative" → forAllSafe((x: A, y: A, z: A) =>
      ((x |+| y) |+| z) === (x |+| (y |+| z))
    ),
    "combineN(a, 1) === a" → forAllSafe((a: A) =>
      A.combineN(a, 1) === a
    ),
    "combineN(a, 2) === a |+| a" → forAllSafe((a: A) =>
      A.combineN(a, 2) === (a |+| a)
    )
  )

  def monoid(implicit A: Monoid[A]) = new GroupProperties(
    name = "monoid",
    parent = Some(semigroup),
    "left identity" → forAllSafe((x: A) =>
      (A.empty |+| x) === x
    ),
    "right identity" → forAllSafe((x: A) =>
      (x |+| A.empty) === x
    ),
    "combineN(a, 0) === id" → forAllSafe((a: A) =>
      A.combineN(a, 0) === A.empty
    ),
    "combineAll(Nil) === id" → forAllSafe((a: A) =>
      A.combineAll(Nil) === A.empty
    ),
    "isId" → forAllSafe((x: A) =>
      (x === A.empty) === (x.isEmpty)
    )
  )

  def cMonoid(implicit A: CMonoid[A]) = new GroupProperties(
    name = "commutative monoid",
    parent = Some(monoid),
    "commutative" → forAllSafe((x: A, y: A) =>
      (x |+| y) === (y |+| x)
    )
  )

  def group(implicit A: Group[A]) = new GroupProperties(
    name = "group",
    parent = Some(monoid),
    "left inverse" → forAllSafe((x: A) =>
      A.empty === (x.inverse |+| x)
    ),
    "right inverse" → forAllSafe((x: A) =>
      A.empty === (x |+| x.inverse)
    )
  )

  def abGroup(implicit A: AbGroup[A]) = new GroupProperties(
    name = "abelian group",
    parent = Some(group),
    "commutative" → forAllSafe((x: A, y: A) =>
      (x |+| y) === (y |+| x)
    )
  )


  // additive groups

  def additiveSemigroup(implicit A: AdditiveSemigroup[A]) = new AdditiveProperties(
    base = semigroup(A.additive),
    parent = None,
    "sumN(a, 1) === a" → forAllSafe((a: A) =>
      A.sumN(a, 1) === a
    ),
    "sumN(a, 2) === a + a" → forAllSafe((a: A) =>
      A.sumN(a, 2) === (a + a)
    )
  )

  def additiveMonoid(implicit A: AdditiveMonoid[A]) = new AdditiveProperties(
    base = monoid(A.additive),
    parent = Some(additiveSemigroup),
    "sumN(a, 0) === zero" → forAllSafe((a: A) =>
      A.sumN(a, 0) === A.zero
    ),
    "sum(Nil) === zero" → forAllSafe((a: A) =>
      A.sum(Nil) === A.zero
    ),
    "isZero" → forAllSafe((a: A) =>
      a.isZero === (a === A.zero)
    )
  )


  def additiveCMonoid(implicit A: AdditiveCMonoid[A]) = new AdditiveProperties(
    base = cMonoid(A.additive),
    parent = Some(additiveMonoid)
  )

  def additiveGroup(implicit A: AdditiveGroup[A]) = new AdditiveProperties(
    base = group(A.additive),
    parent = Some(additiveMonoid),
    "minus consistent" → forAllSafe((x: A, y: A) =>
      (x - y) === (x + (-y))
    )
  )

  def additiveAbGroup(implicit A: AdditiveAbGroup[A]) = new AdditiveProperties(
    base = abGroup(A.additive),
    parent = Some(additiveGroup)
  )


  // property classes

  class GroupProperties(
    name: String,
    parent: Option[GroupProperties],
    props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props: _*)

  class AdditiveProperties(
    val base: GroupProperties,
    val parent: Option[AdditiveProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val name = base.name
    val bases = Seq("base" → base)
  }

}

// vim: expandtab:ts=2:sw=2
