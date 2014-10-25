package spire.algebra.lattice

import scala.{specialized => sp}

trait JoinSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] {
  def join(lhs: A, rhs: A): A
}

trait MeetSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] {
  def meet(lhs: A, rhs: A): A
}

trait Lattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A]
    extends JoinSemilattice[A] with MeetSemilattice[A]

trait BoundedJoinSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends JoinSemilattice[A] {
  def one: A
}

trait BoundedMeetSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends JoinSemilattice[A] {
  def zero: A
}

trait BoundedLattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A]
    extends BoundedMeetSemilattice[A] with BoundedJoinSemilattice[A]
