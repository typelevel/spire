package spire.algebra

import annotation.tailrec
import scala.{specialized => spec}

import java.lang.Math

import spire.math._
import spire.macrosk.Ops


/**
 * Rig is a ring whose additive structure doesn't have an inverse (ie. it is
 * monoid, not a group). Put another way, a Rig is a Ring without a negative.
 */
trait Rig[@spec(Int,Long,Float,Double) A] extends Semiring[A] with AdditiveMonoid[A] with MultiplicativeMonoid[A] {
  override def pow(a:A, n:Int):A =
    if (n < 0) sys.error("illegal exponent: %s" format n)
    else _pow(a, n, one)

  @tailrec private final def _pow(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _pow(times(a, a), n / 2, times(sofar, a))
    else _pow(times(a, a), n / 2, sofar)
}

object Rig {
  @inline final def apply[A](implicit r:Rig[A]): Rig[A] = r

  implicit def intervalIsRig[A: Order: Ring] = new IntervalIsRig[A] {
    val o = Order[A]
    val r = Ring[A]
  }
}

// we don't support Ring[Interval[A]] due to the lack of reliable inverses
// (which is due to the dependency problem for interval arithmetic).
trait IntervalIsRig[A] extends Rig[Interval[A]] {
  implicit def o: Order[A]
  implicit def r: Ring[A]

  def one: Interval[A] = Interval.point(r.one)
  def plus(a:Interval[A], b:Interval[A]): Interval[A] = a + b
  override def pow(a:Interval[A], b:Int):Interval[A] = a.pow(b)
  override def times(a:Interval[A], b:Interval[A]): Interval[A] = a * b
  def zero: Interval[A] = Interval.point(r.zero)
}
