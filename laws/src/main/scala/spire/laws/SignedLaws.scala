package spire.laws

import spire.algebra.{Sign, Signed}

trait SignedLaws[A] extends OrderLaws[A] {
  override implicit def E: Signed[A]

  def absNonnegative(x: A): IsEq[Boolean] =
    (E.sign(E.abs(x)) != Sign.Negative) <=> true

  def signumRange(x: A): IsEq[Boolean] =
    (E.signum(x).abs <= 1) <=> true

  def signumIsSignToInt(x: A): IsEq[Int] =
    E.signum(x) <=> E.sign(x).toInt
}

object SignedLaws {
  def apply[A:Signed]: SignedLaws[A] =
    new SignedLaws[A] { def E: Signed[A] = implicitly }
}
