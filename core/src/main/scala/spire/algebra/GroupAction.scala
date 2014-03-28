package spire.algebra

/**
 * A (left) group action of `G` on `P` is simply the implementation of
 * a method `actl(g, p)`, or `g +> p` in additive notation, such that:
 * 
 * 1. `(g |+| h) +> p === g +> (h +> p)` for all `g`, `h` in `G` and `p` in `P`.
 * 
 * 2. `scalar.id +> p === p` for all `p` in `P`.
 */
trait GroupAction[P, G] {
  def scalar: Group[G]

  def actl(g: G, p: P): P
  def actr(p: P, g: G): P = actl(g, p)
}

object GroupAction {
  implicit def unboundOps[G](g: G)(implicit ev: GroupAction[_, G]) =
    new GroupActionUnboundOps(g)
}

trait AdditiveGroupAction[P, G] {
  def scalar: AdditiveGroup[G]

  def gplusl(g: G, p: P): P
  def gplusr(p: P, g: G): P = gplusl(g, p)
}

object AdditiveGroupAction {
  implicit def unboundOps[G](g: G)(implicit ev: AdditiveGroupAction[_, G]) =
    new AdditiveGroupActionUnboundOps(g)
}

trait MultiplicativeGroupAction[P, G] {
  def scalar: MultiplicativeGroup[G]

  def gtimesl(g: G, p: P): P
  def gtimesr(p: P, g: G): P = gtimesl(g, p)
}

object MultiplicativeGroupAction {
  implicit def unboundOps[G](g: G)(implicit ev: MultiplicativeGroupAction[_, G]) =
    new MultiplicativeGroupActionUnboundOps(g)
}

import spire.macrosk.Ops

final class GroupActionUnboundOps[G](lhs: G)(implicit ev: GroupAction[_, G]) {
  def |+|(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def |-|(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def inverse(): G = macro Ops.unopWithScalar[G]
}

final class AdditiveGroupActionUnboundOps[G](lhs: G)(implicit ev: AdditiveGroupAction[_, G]) {
  def +(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def -(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def unary_-(): G = macro Ops.unopWithScalar[G]
}

final class MultiplicativeGroupActionUnboundOps[G](lhs: G)(implicit ev: MultiplicativeGroupAction[_, G]) {
  def *(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def /(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def reciprocal(): G = macro Ops.unopWithScalar[G]
}
