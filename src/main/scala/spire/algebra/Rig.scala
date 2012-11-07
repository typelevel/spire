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
trait Rig[@spec(Int,Long,Float,Double) A] {
  def zero:A
  def one:A
  def plus(a:A, b:A):A
  def times(a:A, b:A):A

  def additive:Monoid[A] = new AdditiveMonoid[A]()(this)
  def multiplicative:Monoid[A] = new MultiplicativeMonoid[A]()(this)

  def pow(a:A, n:Int):A =
    if (n < 0) sys.error("illegal exponent: %s" format n)
    else _pow(a, n, one)

  @tailrec private final def _pow(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _pow(times(a, a), n / 2, times(sofar, a))
    else _pow(times(a, a), n / 2, sofar)
}

final class RigOps[A](lhs:A)(implicit ev:Rig[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
  def *(rhs:A): A = macro Ops.binop[A, A]

  def pow(rhs:Int) = macro Ops.binop[Int, A]
  def **(rhs:Int) = macro Ops.binop[Int, A]
}

object Rig {
  implicit object UIntIsRing extends UIntIsRig
  implicit object ULongIsRing extends ULongIsRig
  implicit def ringIsRig[@spec(Int,Long,Float,Double) A: Ring]: Rig[A] = Ring[A]

  @inline final def apply[A](implicit r:Rig[A]):Rig[A] = r
}

trait UIntIsRig extends Rig[UInt] {
  def one: UInt = UInt(1)
  def plus(a:UInt, b:UInt): UInt = a + b
  override def pow(a:UInt, b:Int): UInt = a ** b
  override def times(a:UInt, b:UInt): UInt = a * b
  def zero: UInt = UInt(0)
}

trait ULongIsRig extends Rig[ULong] {
  def one: ULong = ULong(1)
  def plus(a:ULong, b:ULong): ULong = a + b
  override def pow(a:ULong, b:Int): ULong = a ** b
  override def times(a:ULong, b:ULong): ULong = a * b
  def zero: ULong = ULong(0)
}

