package spire.algebra

import scala.{ specialized => spec }

/**
 * A (left) group action of `G` on `P` is simply the implementation of
 * a method `actl(g, p)`, or `g +> p` in additive notation, such that:
 * 
 * 1. `(g |+| h) +> p === g +> (h +> p)` for all `g`, `h` in `G` and `p` in `P`.
 * 
 * 2. `scalar.id +> p === p` for all `p` in `P`.
 */
trait GroupAction[@spec(Int) P, G] {
  def actl(g: G, p: P): P
  def actr(p: P, g: G): P
}

object GroupAction {
  @inline def apply[P, G](G: GroupAction[P, G]) = G
  @inline def additive[P, G](G: AdditiveGroupAction[P, G]) = G.additive
  @inline def multiplicative[P, G](G: MultiplicativeGroupAction[P, G]) = G.multiplicative
}

trait AdditiveGroupAction[@spec(Int) P, G] { self =>
  def additive: GroupAction[P, G] = new GroupAction[P, G] {
    def actl(g: G, p: P): P = self.gplusl(g, p)
    def actr(p: P, g: G): P = self.gplusr(p, g)
  }

  def gplusl(g: G, p: P): P
  def gplusr(p: P, g: G): P
}

trait MultiplicativeGroupAction[@spec(Int) P, G] { self =>
  def multiplicative: GroupAction[P, G] = new GroupAction[P, G] {
    def actl(g: G, p: P): P = self.gtimesl(g, p)
    def actr(p: P, g: G): P = self.gtimesr(p, g)
  }

  def gtimesl(g: G, p: P): P
  def gtimesr(p: P, g: G): P
}
