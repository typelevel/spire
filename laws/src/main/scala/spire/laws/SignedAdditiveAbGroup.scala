package spire.laws

import spire.algebra.{AdditiveAbGroup, Signed}

trait SignedAdditiveAbGroupLaws[A] extends SignedLaws[A] with AdditiveAbGroupLaws[A] {

  def absNegate(x: A): IsEq[A] =
    E.abs(x) <=> E.abs(S.negate(x))
}

object SignedAdditiveAbGroupLaws {
  def apply[A:Signed:AdditiveAbGroup]: SignedAdditiveAbGroupLaws[A] =
    new SignedAdditiveAbGroupLaws[A] { def E = implicitly; def S = implicitly }
}
