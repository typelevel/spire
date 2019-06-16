package spire
package laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import InvalidTestException._

object ActionLaws {
  def apply[G: Eq: Arbitrary, A: Eq: Arbitrary] = new ActionLaws[G, A] {
    val scalarLaws = GroupLaws[G]
    def EquA = Eq[A]
    def ArbA = implicitly[Arbitrary[A]]
  }
}

trait ActionLaws[G, A] extends Laws {

  val scalarLaws: GroupLaws[G]

  import scalarLaws.{ Arb => ArG }

  implicit def EquA: Eq[A]
  implicit def ArbA: Arbitrary[A]

    def leftSemigroupAction(implicit G: LeftAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "leftSemigroupAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,

    "left compatibility" -> forAllSafe { (g: G, h: G, a: A) =>
      ((g |+| h) |+|> a) === (g |+|> (h |+|> a))
    }
  )

  def rightSemigroupAction(implicit G: RightAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "rightSemigroupAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,

    "right compatibility" -> forAllSafe { (a: A, g: G, h: G) =>
      (a <|+| (g |+| h)) === ((a <|+| g) <|+| h)
    }
  )

  def semigroupAction(implicit G: Action[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "semigroupAction",
    sl = _.semigroup(G0),
    parents = Seq(leftSemigroupAction, rightSemigroupAction)
  )

  def leftMonoidAction(implicit G: LeftAction[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "leftMonoidAction",
    sl = _.monoid(G0),
    parents = Seq(leftSemigroupAction),

    "left identity" -> forAllSafe { (a: A) =>
      (G0.empty |+|> a) === a
    }
  )

  def rightMonoidAction(implicit G: RightAction[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "rightMonoidAction",
    sl = _.monoid(G0),
    parents = Seq(rightSemigroupAction),

    "right identity" -> forAllSafe { (a: A) =>
      (a <|+| G0.empty) === a
    }
  )

  def monoidAction(implicit G: Action[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "monoidAction",
    sl = _.monoid(G0),
    parents = Seq(leftMonoidAction, rightMonoidAction)
  )

  def groupAction(implicit G: Action[A, G], G0: Group[G]) = new ActionProperties(
    name = "groupAction",
    sl = _.group(G0),
    parents = Seq(monoidAction),

    "left and right action compatibility" -> forAllSafe { (a: A, g: G) =>
      (a <|+| g) === (g.inverse |+|> a)
    }
  )

  def additiveMonoidAction(implicit G: AdditiveAction[A, G], G0: AdditiveMonoid[G]) = new AdditiveProperties(
    base = monoidAction(G.additive, G0.additive),
    parent = None
  )

  def multiplicativeMonoidAction(implicit G: MultiplicativeAction[A, G], G0: MultiplicativeMonoid[G]) = new MultiplicativeProperties(
    base = monoidAction(G.multiplicative, G0.multiplicative),
    parent = None
  )

  class ActionProperties(
    val name: String,
    val sl: scalarLaws.type => scalarLaws.RuleSet,
    val parents: Seq[ActionProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq("scalar" -> sl(scalarLaws))
  }

  class AdditiveProperties(
    val base: ActionProperties,
    val parent: Option[AdditiveProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val name = base.name
    val bases = Seq("base" -> base)
  }

  class MultiplicativeProperties(
    val base: ActionProperties,
    val parent: Option[MultiplicativeProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val name = base.name
    val bases = Seq("base" -> base)
  }
}
