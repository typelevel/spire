package spire.laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object ActionLaws {
  def apply[G: Eq: Arbitrary, A: Eq: Arbitrary] = new ActionLaws[G, A] {
    val scalarLaws = GroupLaws[G]
    def EquA = Eq[A]
    def ArbA = implicitly[Arbitrary[A]]
  }
}

trait ActionLaws[G, A] extends Laws {

  val scalarLaws: GroupLaws[G]

  import scalarLaws.{ Equ => EqG, Arb => ArG }

  implicit def EquA: Eq[A]
  implicit def ArbA: Arbitrary[A]

  def leftSemigroupoidPartialAction(implicit G: LeftPartialAction[A, G], G0: Semigroupoid[G]) = new ActionProperties(
    name = "leftSemigroupAction",
    sl = _.semigroupoid(G0),
    parents = Seq.empty,

    "left compatibility" → forAll { (g: G, h: G, a: A) =>
      ( (h ?+|> a) && (g ?+? h) ) ==>
      ((g |+|! h) ?+|> a) && ((g |+|! h) !|+|> a) === (g !|+|> (h !|+|> a))
    }
  )

  def rightSemigroupoidPartialAction(implicit G: RightPartialAction[A, G], G0: Semigroupoid[G]) = new ActionProperties(
    name = "rightSemigroupAction",
    sl = _.semigroupoid(G0),
    parents = Seq.empty,

    "right compatibility" → forAll { (g: G, h: G, a: A) =>
      ( (a <|+? g) && (g ?+? h) ) ==>
      (a <|+? (g |+|! h)) && ((a <|+|! (g |+|! h)) === ((a <|+|! g) <|+|! h))
    }
  )

  def semigroupoidPartialAction(implicit G: PartialAction[A, G], G0: Semigroupoid[G]) = new ActionProperties(
    name = "semigroupAction",
    sl = _.semigroupoid(G0),
    parents = Seq(leftSemigroupoidPartialAction, rightSemigroupoidPartialAction)
  )

  def leftPartialMonoidPartialAction(implicit G: LeftPartialAction[A, G], G0: PartialMonoid[G]) = new ActionProperties(
    name = "leftPartialMonoidPartialAction",
    sl = _.partialMonoid(G0),
    parents = Seq(leftSemigroupoidPartialAction),

    "left action identity" → forAll { (g: G, a: A) =>
      (g ?+|> a) ==>
      ((g.rightId ?+|> a) && ((g.rightId !|+|> a) === a))
    }
  )

  def rightPartialMonoidPartialAction(implicit G: RightPartialAction[A, G], G0: PartialMonoid[G]) = new ActionProperties(
    name = "rightPartialMonoidPartialAction",
    sl = _.partialMonoid(G0),
    parents = Seq(rightSemigroupoidPartialAction),

    "right action identity" → forAll { (g: G, a: A) =>
      (a <|+? g) ==>
      ((a <|+? g.leftId) && ((a <|+|! g.leftId) === a))
    }
  )

  def partialMonoidPartialAction(implicit G: PartialAction[A, G], G0: PartialMonoid[G]) = new ActionProperties(
    name = "partialMonoidPartialAction",
    sl = _.partialMonoid(G0),
    parents = Seq(semigroupoidPartialAction, leftPartialMonoidPartialAction, rightPartialMonoidPartialAction)
  )

  def groupoidPartialAction(implicit G: PartialAction[A, G], G0: Groupoid[G]) = new ActionProperties(
    name = "groupoidPartialAction",
    sl = _.groupoid(G0),
    parents = Seq(partialMonoidPartialAction),

    "left and right partial action compatibility" → forAll { (a: A, g: G) =>
      (a <|+? g) ==>
      ((g.inverse ?+|> a) && ((a <|+|! g) === (g.inverse !|+|> a)))
    }
  )


  def leftSemigroupPartialAction(implicit G: LeftPartialAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "leftSemigroupPartialAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,

    "left compatibility" → forAll { (g: G, h: G, a: A) =>
      ( (h ?+|> a) && ((g |+| h) ?+|> a) ) ==>
      (((g |+| h) !|+|> a) === (g !|+|> (h !|+|> a)))
    }
  )

  def rightSemigroupPartialAction(implicit G: RightPartialAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "rightSemigroupPartialAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,

    "right compatibility" → forAll { (a: A, g: G, h: G) =>
      ( (a <|+? g) && (a <|+? (g |+| h)) ) ==>
      ((a <|+|! (g |+| h)) === ((a <|+|! g) <|+|! h))
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

    "left identity" → forAll { (a: A) =>
      (G0.id ?+|> a) && ((G0.id !|+|> a) === a)
    }
  )

  def rightMonoidPartialAction(implicit G: RightPartialAction[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "rightMonoidPartialAction",
    sl = _.monoid(G0),
    parents = Seq(rightSemigroupPartialAction),

    "right identity" → forAll { (a: A) =>
      (a <|+? G0.id) && ((a <|+|! G0.id) === a)
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

    "right -> left action compatibility" → forAll { (a: A, g: G) =>
      !(a <|+? g) || ((g ?+|> a) && ((a <|+|! g) === (g.inverse !|+|> a)))
    },

    "left -> right action compatibility" → forAll { (a: A, g: G) =>
      !(g ?+|> a) || ((a <|+? g) && ((g !|+|> a) === (a <|+|! g.inverse)))
    }
  )

  def leftSemigroupAction(implicit G: LeftAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "leftSemigroupAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,

    "left compatibility" → forAll { (g: G, h: G, a: A) =>
      ((g |+| h) |+|> a) === (g |+|> (h |+|> a))
    }
  )

  def rightSemigroupAction(implicit G: RightAction[A, G], G0: Semigroup[G]) = new ActionProperties(
    name = "rightSemigroupAction",
    sl = _.semigroup(G0),
    parents = Seq.empty,

    "right compatibility" → forAll { (a: A, g: G, h: G) =>
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

    "left identity" → forAll { (a: A) =>
      (G0.id |+|> a) === a
    }
  )

  def rightMonoidAction(implicit G: RightAction[A, G], G0: Monoid[G]) = new ActionProperties(
    name = "rightMonoidAction",
    sl = _.monoid(G0),
    parents = Seq(rightSemigroupAction),

    "right identity" → forAll { (a: A) =>
      (a <|+| G0.id) === a
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

    "left and right action compatibility" → forAll { (a: A, g: G) =>
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
    val bases = Seq("scalar" → sl(scalarLaws))
  }

  class AdditiveProperties(
    val base: ActionProperties,
    val parent: Option[AdditiveProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val name = base.name
    val bases = Seq("base" → base)
  }

  class MultiplicativeProperties(
    val base: ActionProperties,
    val parent: Option[MultiplicativeProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val name = base.name
    val bases = Seq("base" → base)
  }
}
