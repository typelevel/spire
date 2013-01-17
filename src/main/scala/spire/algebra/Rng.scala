package spire.algebra

import annotation.tailrec
import scala.{specialized => spec}

import java.lang.Math

import spire.math._
import spire.macrosk.Ops

/**
 * Rng is a ring whose multiplicative structure doesn't have an identity
 * (i.e. it is semigroup, not a monoid). Put another way, a Rng is a Ring
 * without an identity.
 */
trait Rng[@spec(Int,Long,Float,Double) A] extends Semiring[A] with AdditiveAbGroup[A]

object Rng extends RngProductImplicits {
  implicit def ringIsRng[@spec(Int,Long,Float,Double) A: Ring]: Rng[A] = Ring[A]

  @inline final def apply[A](implicit r:Rng[A]):Rng[A] = r
}
