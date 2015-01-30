package spire.algebra
package partial

import spire.util.Opt

/**
  * A left partial action of a semigroupoid `G` on `P` is the implementation of
  * a method `partialActl(g, p)`, or `g ?|+|> p` returning `Opt[P]`, such that:
  * 
  * 1. for all `g`, `h` in `G`, `p` in `P` such that `g |+|? h` and `h ?|+|> p` are defined,
  * 
  *    `((g |+|? h).get ?|+|> p).get === (g ?|+|> (h ?|+|> p).get).get` with all operations
  *    defined.
  * 
  * In addition, if `G` is a partial monoid, the following relation holds:
  * 
  * 2. for all `g` in `G` and `p` in `P` such that `g ?|+|> p` is defined:
  * 
  * `(g.rightId ?|+|> p).get === p`, the operation `?|+|>` being defined.
  */
trait LeftPartialAction[P, G] extends Any {
  def actlIsDefined(g: G, p: P): Boolean
  def partialActl(g: G, p: P): Opt[P]
}

object LeftPartialAction {
  @inline final def apply[P, G](implicit G: LeftPartialAction[P, G]) = G

  implicit def fromLeftAction[P, G](implicit G: LeftAction[P, G]): LeftPartialAction[P, G] =
    new LeftPartialAction[P, G] {
      def actlIsDefined(g: G, p: P) = true
      def partialActl(g: G, p: P): Opt[P] = Opt(G.actl(g, p))
    }
}

/**
  * A right partial action of a semigroupoid `G` on `P` is the implementation of
  * a method `partialActr(p, g)`, or `p <|+|? g` returning `Opt[P]`, such that:
  * 
  * 1. for all `g`, `h` in `G`, `p` in `P` such that `g |+|? h` and `p <|+|? g` are defined,
  * 
  *    `(p <|+|? (g |+|? h).get).get === ((p <|+|? g).get |+|? h).get`, 
  *    and all operations are defined.
  * 
  * In addition, if `G` is a partial monoid, the following relation holds:
  * 
  * 2. for all `g` in `G` and `p` in `P` such that `p <|+|? g` is defined:
  * 
  * `(p <|+|? g.leftId).get === p`, the operation `<|+|?` being defined.
  */

trait RightPartialAction[P, G] extends Any {
  def actrIsDefined(p: P, g: G): Boolean
  def partialActr(p: P, g: G): Opt[P]
}

object RightPartialAction {
  @inline final def apply[P, G](implicit G: RightPartialAction[P, G]) = G

  implicit def fromRightAction[P, G](implicit G: RightAction[P, G]): RightPartialAction[P, G] =
    new RightPartialAction[P, G] {
      def actrIsDefined(p: P, g: G) = true
      def partialActr(p: P, g: G): Opt[P] = Opt(G.actr(p, g))
    }
}

trait PartialAction[P, G] extends Any
    with LeftPartialAction[P, G] with RightPartialAction[P, G]

object PartialAction {
  @inline final def apply[P, G](implicit G: PartialAction[P, G]) = G

  implicit def fromAction[P, G](implicit G: Action[P, G]): PartialAction[P, G] =
    new PartialAction[P, G] {
      def actlIsDefined(g: G, p: P) = true
      def partialActl(g: G, p: P): Opt[P] = Opt(G.actl(g, p))
      def actrIsDefined(p: P, g: G) = true
      def partialActr(p: P, g: G): Opt[P] = Opt(G.actr(p, g))
    }

}
