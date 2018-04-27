package spire.laws.shadows

import spire.algebra.{Conjugation, Eq, MultiplicativeCMonoid}
import spire.syntax.conjugation._

trait ShadowConjugation[A, S] extends Conjugation[Shadow[A, S]] {
  implicit def A: Conjugation[A]
  implicit def S: Conjugation[S]

  implicit val shadowing: Shadowing[A, S]
  import shadowing._

  def conjugate(x: Shadow[A, S]): Shadow[A, S] = Shadow(x.a.conjugate, checked(x.s.conjugate))
}
