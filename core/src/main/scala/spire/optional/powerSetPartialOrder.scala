package spire.optional

import spire.algebra.PartialOrder

object powerSetPartialOrder {
  /** Set partial order defined as follows:
    *
    * S <= T if S is a subset of T.
    */
  class PowerSetPartialOrder[A] extends PartialOrder[Set[A]] {
    override def eqv(x: Set[A], y: Set[A]) = (x == y)
    override def lteqv(x: Set[A], y: Set[A]) = x.subsetOf(y)
    override def lt(x: Set[A], y: Set[A]) = x.subsetOf(y) && x != y
    override def gteqv(x: Set[A], y: Set[A]) = y.subsetOf(x)
    override def gt(x: Set[A], y: Set[A]) = y.subsetOf(x) && x != y

    def partialCompare(x: Set[A], y: Set[A]): Double = {
      if (eqv(x, y))
        0.0
      else if (lt(x, y))
        -1.0
      else if (gt(x, y))
        1.0
      else
        Double.NaN
    }
  }
  implicit def powerSetPartialOrder[A]: PartialOrder[Set[A]] = new PowerSetPartialOrder[A]
}
