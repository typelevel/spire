package spire.algebra

import annotation.tailrec
import scala.{specialized => spec}

import java.lang.Math

import spire.math._
import spire.macrosk.Ops


/**
 * Ring represents a set (A) that is a group over addition (+) and a monoid
 * over multiplication (*). Aside from this, the multiplication must distribute
 * over addition.
 *
 * Ring implements some methods (for example fromInt) in terms of other more
 * fundamental methods (zero, one and plus). Where possible, these methods
 * should be overridden by more efficient implementations.
 */
trait Ring[@spec(Int,Long,Float,Double) A] extends Semiring[A] with Rig[A] with Rng[A] with AdditiveAbGroup[A] {
  def fromInt(n: Int): A =
    if (n < 0) _fromInt(negate(one), -n, zero)
    else _fromInt(one, n, zero)
  
  @tailrec private def _fromInt(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _fromInt(plus(a, a), n / 2, plus(sofar, a))
    else _fromInt(plus(a, a), n / 2, sofar)
}

object Ring {
  @inline final def apply[A](implicit r:Ring[A]):Ring[A] = r
}
