package spire.algebra

import scala.{ specialized => spec }

/**
 * A (left) semigroup/monoid/group action of `G` on `P` is simply the implementation of
 * a method `actl(g, p)`, or `g +> p` in additive notation, such that:
 * 
 * 1. `(g |+| h) |+|> p === g |+|> (h |+|> p)` for all `g`, `h` in `G` and `p` in `P`.
 * 
 * 2. `id |+|> p === p` for all `p` in `P` (if `id` is defined)
 */
trait Action[@spec(Int) P, G] extends Any {
  def actl(g: G, p: P): P
  def actr(p: P, g: G): P
}

object Action {
  @inline def apply[P, G](G: Action[P, G]) = G
  @inline def additive[P, G](G: AdditiveAction[P, G]) = G.additive
  @inline def multiplicative[P, G](G: MultiplicativeAction[P, G]) = G.multiplicative
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
