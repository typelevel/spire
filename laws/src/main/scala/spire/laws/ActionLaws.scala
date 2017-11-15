package spire
package laws

import spire.algebra._

trait LeftActionLaws[A, G] {
  implicit def A: LeftAction[A, G]

  def leftCompatibility(g: G, h: G, a: A, G: Semigroup[G]): IsEq[A] =
    A.actl(G.combine(g, h), a) <=> A.actl(g, A.actl(h, a))

  def leftIdentity(a: A, G: Monoid[G]): IsEq[A] =
    A.actl(G.empty, a) <=> a

}

trait RightActionLaws[A, G] {
  implicit def A: RightAction[A, G]

  def rightCompatibility(a: A, g: G, h: G, G: Semigroup[G]): IsEq[A] =
    A.actr(a, G.combine(g, h)) <=> A.actr(A.actr(a, g), h)

  def rightIdentity(a: A, G: Monoid[G]): IsEq[A] =
    A.actr(a, G.empty) <=> a

}

trait ActionLaws[A, G] {
  implicit def A: Action[A, G]

  def leftRightCompatibility(a: A, g: G, G: Group[G]): IsEq[A] =
    A.actr(a, g) <=> A.actl(G.inverse(g), a)

}
