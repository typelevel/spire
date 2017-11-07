package spire.laws

trait SignedAdditiveAbGroupLaws[A] extends SignedLaws[A] with AdditiveAbGroupLaws[A] {

  def absNegate(x: A): IsEq[A] =
    E.abs(x) <=> E.abs(S.negate(x))
}
