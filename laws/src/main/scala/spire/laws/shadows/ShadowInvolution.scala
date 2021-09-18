package spire.laws.shadows

import spire.algebra.Involution
import spire.syntax.involution._

trait ShadowInvolution[A, S] extends Involution[Shadow[A, S]] {
  implicit def A: Involution[A]
  implicit def S: Involution[S]

  implicit val shadowing: Shadowing[A, S]
  import shadowing._

  def adjoint(x: Shadow[A, S]): Shadow[A, S] = Shadow(x.a.adjoint, checked(x.s.adjoint))
}
