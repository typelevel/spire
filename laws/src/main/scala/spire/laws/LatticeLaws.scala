package spire
package laws

import spire.algebra._
import spire.algebra.lattice._

trait JoinSemilatticeLaws[A] {
  implicit def A: JoinSemilattice[A]

  def joinAssociative(x: A, y: A, z: A): IsEq[A] =
    A.join(A.join(x, y), z) <=> A.join(x, A.join(y, z))

  def joinCommutative(x: A, y: A): IsEq[A] =
    A.join(x, y) <=> A.join(y, x)

  def joinIdempotent(x: A): IsEq[A] =
    A.join(x, x) <=> x

  def joinPartialOrder(x: A, y: A, po: PartialOrder[A]): IsEq[Boolean] =
    po.lteqv(x, y) <=> po.eqv(y, A.join(x, y))
}


trait MeetSemilatticeLaws[A] {
  implicit def A: MeetSemilattice[A]

  def meetAssociative(x: A, y: A, z: A): IsEq[A] =
    A.meet(A.meet(x, y), z) <=> A.meet(x, A.meet(y, z))

  def meetCommutative(x: A, y: A): IsEq[A] =
    A.meet(x, y) <=> A.meet(y, x)

  def meetIdempotent(x: A): IsEq[A] =
    A.meet(x, x) <=> x

  def meetPartialOrder(x: A, y: A, po: PartialOrder[A]): IsEq[Boolean] =
    po.lteqv(x, y) <=> po.eqv(x, A.meet(x, y))
}

trait LatticeLaws[A] extends JoinSemilatticeLaws[A] with MeetSemilatticeLaws[A] {
  implicit def A: Lattice[A]

  def absorptionJoinMeet(x: A, y: A): IsEq[A] =
    A.join(x, A.meet(x, y)) <=> x

  def absorptionMeetJoin(x: A, y: A): IsEq[A] =
    A.meet(x, A.join(x, y)) <=> x
}

trait BoundedJoinSemilatticeLaws[A] extends JoinSemilatticeLaws[A] {
  implicit def A: BoundedJoinSemilattice[A]

  def joinIdentity(x: A): IsEq[A] =
    A.join(x, A.zero) <=> x

  def zeroPartialOrder(x: A, po: PartialOrder[A]): IsEq[Boolean] =
    po.lteqv(A.zero, x) <=> true
}

trait BoundedMeetSemilatticeLaws[A] extends MeetSemilatticeLaws[A] {
  implicit def A: BoundedMeetSemilattice[A]

  def meetIdentity(x: A): IsEq[A] =
    A.meet(x, A.one) <=> x

  def onePartialOrder(x: A, po: PartialOrder[A]): IsEq[Boolean] =
    po.lteqv(x, A.one) <=> true
}

trait BoundedBelowLatticeLaws[A] extends BoundedJoinSemilatticeLaws[A] with LatticeLaws[A] {
  implicit def A: BoundedJoinSemilattice[A] with Lattice[A]
}

trait BoundedAboveLatticeLaws[A] extends BoundedMeetSemilatticeLaws[A] with LatticeLaws[A] {
  implicit def A: BoundedMeetSemilattice[A] with Lattice[A]
}

trait BoundedLatticeLaws[A] extends BoundedBelowLatticeLaws[A] with BoundedAboveLatticeLaws[A] {
  implicit def A: BoundedLattice[A]
}


// TODO: missing the distributive lattice stuff
