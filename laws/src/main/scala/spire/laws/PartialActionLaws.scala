package spire
package laws

import spire.algebra._
import spire.algebra.partial._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import InvalidTestException._

object PartialActionLaws {
  def apply[G: Eq: Arbitrary, A: Eq: Arbitrary] = new PartialActionLaws[G, A] {
    val scalarLaws = PartialGroupLaws[G]
    def EquA = Eq[A]
    def ArbA = implicitly[Arbitrary[A]]
  }
}

trait PartialActionLaws[G, A] extends Laws {

  val scalarLaws: PartialGroupLaws[G]

  import scalarLaws.{Arb => ArG}

  implicit def EquA: Eq[A]
  implicit def ArbA: Arbitrary[A]

  def leftSemigroupoidPartialAction(implicit G: LeftPartialAction[A, G], G0: Semigroupoid[G]) = new ActionProperties(
    name = "leftSemigroupAction",
    sl = _.semigroupoid(G0),
    parents = Seq.empty,
    "left compatibility" -> forAllSafe { (g: G, h: G, a: A) =>
      ((h ??|+|> a) && (g |+|?? h)) ==>
        ((g |+|? h).get ??|+|> a) && ((g |+|? h).get ?|+|> a).get === (g ?|+|> (h ?|+|> a).get).get
    }
  )

  def rightSemigroupoidPartialAction(implicit G: RightPartialAction[A, G], G0: Semigroupoid[G]) = new ActionProperties(
    name = "rightSemigroupAction",
    sl = _.semigroupoid(G0),
    parents = Seq.empty,
    "right compatibility" -> forAllSafe { (g: G, h: G, a: A) =>
      ((a <|+|?? g) && (g |+|?? h)) ==>
        (a <|+|?? (g |+|? h).get) && ((a <|+|? (g |+|? h).get).get === ((a <|+|? g).get <|+|? h).get)
    }
  )

  def semigroupoidPartialAction(implicit G: PartialAction[A, G], G0: Semigroupoid[G]) = new ActionProperties(
    name = "semigroupAction",
    sl = _.semigroupoid(G0),
    parents = Seq(leftSemigroupoidPartialAction, rightSemigroupoidPartialAction)
  )

  def groupoidPartialAction(implicit G: PartialAction[A, G], G0: Groupoid[G]) = new ActionProperties(
    name = "groupoidPartialAction",
    sl = _.groupoid(G0),
    parents = Seq(semigroupoidPartialAction),
    "left action identity" -> forAllSafe { (g: G, a: A) =>
      (g ??|+|> a) ==>
        ((g.rightId ??|+|> a) && ((g.rightId ?|+|> a).get === a))
    },
    "right action identity" -> forAllSafe { (g: G, a: A) =>
      (a <|+|?? g) ==>
        ((a <|+|?? g.leftId) && ((a <|+|? g.leftId).get === a))
    },
    "left and right partial action compatibility" -> forAllSafe { (a: A, g: G) =>
      (a <|+|?? g) ==>
        ((g.inverse ??|+|> a) && ((a <|+|? g).get === (g.inverse ?|+|> a).get))
    }
  )

  def leftSemigroupPartialAction(implicit G: LeftPartialAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "leftSemigroupPartialAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,
    "left compatibility" -> forAllSafe { (g: G, h: G, a: A) =>
      ((h ??|+|> a) && ((g |+| h) ??|+|> a)) ==>
        (((g |+| h) ?|+|> a).get === (g ?|+|> (h ?|+|> a).get).get)
    }
  )

  def rightSemigroupPartialAction(implicit G: RightPartialAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "rightSemigroupPartialAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,
    "right compatibility" -> forAllSafe { (a: A, g: G, h: G) =>
      ((a <|+|?? g) && (a <|+|?? (g |+| h))) ==>
        ((a <|+|? (g |+| h)).get === ((a <|+|? g).get <|+|? h).get)
    }
  )

  def semigroupPartialAction(implicit G: PartialAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "semigroupPartialAction",
    sl = _.semigroup(G0),
    parents = Seq(leftSemigroupPartialAction, rightSemigroupPartialAction)
  )

  def leftMonoidPartialAction(implicit G: LeftPartialAction[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "leftMonoidPartialAction",
    sl = _.monoid(G0),
    parents = Seq(leftSemigroupPartialAction),
    "left identity" -> forAllSafe { (a: A) =>
      (G0.empty ??|+|> a) && ((G0.empty ?|+|> a).get === a)
    }
  )

  def rightMonoidPartialAction(implicit G: RightPartialAction[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "rightMonoidPartialAction",
    sl = _.monoid(G0),
    parents = Seq(rightSemigroupPartialAction),
    "right identity" -> forAllSafe { (a: A) =>
      (a <|+|?? G0.empty) && ((a <|+|? G0.empty).get === a)
    }
  )

  def monoidPartialAction(implicit G: PartialAction[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "monoidPartialAction",
    sl = _.monoid(G0),
    parents = Seq(semigroupPartialAction, leftSemigroupPartialAction, rightSemigroupPartialAction)
  )

  def groupPartialAction(implicit G: PartialAction[A, G], G0: Group[G]) = new ActionProperties(
    name = "groupPartialAction",
    sl = _.group(G0),
    parents = Seq(monoidPartialAction),
    "right -> left action compatibility" -> forAllSafe { (a: A, g: G) =>
      !(a <|+|?? g) || ((g ??|+|> a) && ((a <|+|? g).get === (g.inverse ?|+|> a).get))
    },
    "left -> right action compatibility" -> forAllSafe { (a: A, g: G) =>
      !(g ??|+|> a) || ((a <|+|?? g) && ((g ?|+|> a).get === (a <|+|? g.inverse).get))
    }
  )

  class ActionProperties(
    val name: String,
    val sl: scalarLaws.type => scalarLaws.RuleSet,
    val parents: Seq[ActionProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq("scalar" -> sl(scalarLaws))
  }
}
