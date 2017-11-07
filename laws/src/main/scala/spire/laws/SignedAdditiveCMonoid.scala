package spire.laws

import org.scalacheck.Prop.forAll

trait SignedAdditiveCMonoidLaws[A] extends SignedLaws[A] with AdditiveCMonoidLaws[A] {

  def orderedGroup(x: A, y: A, z: A): IsEq[Int] =
    E.compare(x, y) <=> E.compare(S.plus(x, z), S.plus(y, z))

  def triangleInequality(x: A, y: A): IsEq[Boolean] =
    E.lteqv(E.abs(S.plus(x, y)), S.plus(E.abs(x), E.abs(y))) <=> true
}
