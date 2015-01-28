package spire.algebra

import scala.{ specialized => spec }

/**
  * A left partial action of a semigroupoid `G` on `P` is simply the implementation of
  * a method `partialActl(g, p)`, or `g ?|+|> p` returning `Option[P]`, such that:
  * 
  * 1. for all `g`, `h` in `G`, `p` in `P` such that `g |+|? h` and `h ?|+|> p` are defined,
  * 
  *    `(g |+|! h) !|+|> p === g !|+|> (h !|+|> p)`, and all operations are defined.
  * 
  * In addition, if `G` is a partial monoid, the following relation holds:
  * 
  * 2. for all `g` in `G` and `p` in `P` such that `g ?|+|> p` is defined:
  * 
  * `g.rightId !|+|> p === p`, the operation `!|+|>` being defined.
  */
trait LeftPartialAction[@spec(Int) P, G] extends Any {
  def partialActl(g: G, p: P): Option[P]
  def isActlDefined(g: G, p: P): Boolean = partialActl(g, p).nonEmpty
  def forceActl(g: G, p: P): P = partialActl(g, p) match {
    case Some(result) => result
    case None => throw new IllegalArgumentException(s"Action $g |+|> is not compatible with $p")
  }
}

/**
  * A right partial action of a semigroupoid `G` on `P` is simply the implementation of
  * a method `partialActr(p, g)`, or `p <|+|? g` returning `Option[P]`, such that:
  * 
  * 1. for all `g`, `h` in `G`, `p` in `P` such that `g |+|? h` and `p <|+|? g` are defined,
  * 
  *    `p <|+|! (g |+|! h) === (p <|+|! g) |+|! h`, and all operations are defined.
  * 
  * In addition, if `G` is a partial monoid, the following relation holds:
  * 
  * 2. for all `g` in `G` and `p` in `P` such that `p <|+|? g` is defined:
  * 
  * `p <|+|! g.leftId === p`, the operation `<|+|!` being defined.
  */
trait RightPartialAction[@spec(Int) P, G] extends Any {
  def partialActr(p: P, g: G): Option[P]
  def isActrDefined(p: P, g: G): Boolean = partialActr(p, g).nonEmpty
  def forceActr(p: P, g: G): P = partialActr(p, g) match {
    case Some(result) => result
    case None => throw new IllegalArgumentException(s"$p is not compatible with action <|+| $g")
  }
}

/**
  * A partial action which is both a left and right partial action.
  * 
  * If `G` is a groupoid, the following relation holds:
  * 
  * 1. if `p <|+|! g` is defined, then `g.inverse !|+|> p === p <|+|! g` is defined,
  * 2. if `g !|+|> p` is defined, then `p <|+|! g.inverse === g !|+|> p` is defined.
  */
trait PartialAction[@spec(Int) P, G] extends Any with LeftPartialAction[P, G] with RightPartialAction[P, G]

/**
  * A left semigroup action of `G` on `P` is simply the implementation of
  * a method `actl(g, p)`, or `g |+|> p`, such that:
  * 
  * 1. `(g |+| h) |+|> p === g |+|> (h |+|> p)` for all `g`, `h` in `G` and `p` in `P`.
  * 
  * If, in addition, `G` is a monoid, the following relation holds:
  * 
  * 2. `id |+|> p === p` for all `p` in `P`.
  */
trait LeftAction[@spec(Int) P, G] extends Any with LeftPartialAction[P, G] {
  def actl(g: G, p: P): P

  // trivial implementation of the partial left action
  def partialActl(g: G, p: P): Option[P] = Some(actl(g, p))
  override def isActlDefined(g: G, p: P): Boolean = true
  override def forceActl(g: G, p: P): P = actl(g, p)
}

/**
 * A (left) semigroup/monoid/group action of `G` on `P` is simply the implementation of
 * a method `actl(g, p)`, or `g |+|> p`, such that:
 * 
 * 1. `p <|+| (g |+| h) === (p <|+| g) <|+| h` for all `g`, `h` in `G` and `p` in `P`.
 * 
 * 2. `p <|+| id === p` for all `p` in `P` (if `id` is defined, e.g. if `G: Monoid`).
 */
trait RightAction[@spec(Int) P, G] extends Any with RightPartialAction[P, G] {
  def actr(p: P, g: G): P

  // trivial implementation of the partial right action
  def partialActr(p: P, g: G): Option[P] = Some(actr(p, g))
  override def isActrDefined(p: P, g: G): Boolean = true
  override def forceActr(p: P, g: G): P = actr(p, g)
}

/**
  * An action that is both a left and right action; in addition to the left and right
  * action laws, we have:
  * 
  * 1. `p <|+| g === g.inverse |+|> p`
  */
trait Action[@spec(Int) P, G] extends Any with LeftAction[P, G] with RightAction[P, G] with PartialAction[P, G]

object Action {
  @inline def apply[P, G](G: Action[P, G]) = G
  @inline def additive[P, G](G: AdditiveAction[P, G]) = G.additive
  @inline def multiplicative[P, G](G: MultiplicativeAction[P, G]) = G.multiplicative

  @inline def forced[P, G](G: PartialAction[P, G]): Action[P, G] = new Action[P, G] {
    def actl(g: G, p: P): P = G.forceActl(g, p)
    def actr(p: P, g: G): P = G.forceActr(p, g)
  }
}

trait AdditiveAction[@spec(Int) P, G] extends Any { self =>
  def additive: Action[P, G] = new Action[P, G] {
    def actl(g: G, p: P): P = self.gplusl(g, p)
    def actr(p: P, g: G): P = self.gplusr(p, g)
  }

  def gplusl(g: G, p: P): P
  def gplusr(p: P, g: G): P
}

trait MultiplicativeAction[@spec(Int) P, G] extends Any { self =>
  def multiplicative: Action[P, G] = new Action[P, G] {
    def actl(g: G, p: P): P = self.gtimesl(g, p)
    def actr(p: P, g: G): P = self.gtimesr(p, g)
  }

  def gtimesl(g: G, p: P): P
  def gtimesr(p: P, g: G): P
}
