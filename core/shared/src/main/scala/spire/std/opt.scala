package spire
package std

import spire.algebra.Eq
import spire.util.Opt

trait OptInstances {
  implicit def OptEq[A: Eq]: Eq[Opt[A]] = new Eq[Opt[A]] with Serializable {
    def eqv(x: Opt[A], y: Opt[A]): Boolean =
      if (x.isEmpty) y.isEmpty
      else if (y.isEmpty) false
      else Eq[A].eqv(x.ref, y.ref)
  }
}
