package spire.algebra
package lattice

import spire.syntax.order._
import spire.syntax.euclideanRing._

import scala.{specialized => sp}

trait JoinSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any {
  def join(lhs: A, rhs: A): A
}

trait MeetSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any {
  def meet(lhs: A, rhs: A): A
}

trait Lattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with JoinSemilattice[A] with MeetSemilattice[A]

object Lattice {
  def min[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A](implicit ev: Order[A]): Lattice[A] =
    new MinMaxLattice[A]

  def gcd[@sp(Byte, Short, Int, Long) A](implicit ev: EuclideanRing[A]): Lattice[A] =
    new GcdLcmLattice[A]
}

class MinMaxLattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A: Order] extends Lattice[A] {
  def meet(lhs: A, rhs: A): A = lhs min rhs
  def join(lhs: A, rhs: A): A = lhs max rhs
}

class GcdLcmLattice[@sp(Byte, Short, Int, Long) A: EuclideanRing] extends Lattice[A] {
  def meet(lhs: A, rhs: A): A = lhs gcd rhs
  def join(lhs: A, rhs: A): A = lhs lcm rhs
}

trait BoundedJoinSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with JoinSemilattice[A] {
  def zero: A
}

trait BoundedMeetSemilattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with MeetSemilattice[A] {
  def one: A
}

trait BoundedLattice[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Lattice[A] with BoundedMeetSemilattice[A] with BoundedJoinSemilattice[A]
