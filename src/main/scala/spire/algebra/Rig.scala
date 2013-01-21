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

object Rig extends Rig0 with RigProductImplicits {
  implicit def ringIsRig[@spec(Int,Long,Float,Double) A: Ring]: Rig[A] = Ring[A]

  @inline final def apply[A](implicit r:Rig[A]): Rig[A] = r
}

trait Rig0 {
  implicit object BooleanIsRig extends BooleanIsRig
  implicit object UByteIsRig extends UByteIsRig
  implicit object UShortIsRig extends UShortIsRig
  implicit object UIntIsRig extends UIntIsRig
  implicit object ULongIsRig extends ULongIsRig
  implicit object NaturalIsRig extends NaturalIsRig
}

trait BooleanIsRig extends Rig[Boolean] {
  def one: Boolean = true
  def plus(a:Boolean, b:Boolean): Boolean = a || b
  override def pow(a:Boolean, b:Int): Boolean = a
  override def times(a:Boolean, b:Boolean): Boolean = a && b
  def zero: Boolean = false
}

trait UByteIsRig extends Rig[UByte] {
  def one: UByte = UByte(1)
  def plus(a:UByte, b:UByte): UByte = a + b
  override def pow(a:UByte, b:Int): UByte = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** UByte(b)
  }
  override def times(a:UByte, b:UByte): UByte = a * b
  def zero: UByte = UByte(0)
}

trait UShortIsRig extends Rig[UShort] {
  def one: UShort = UShort(1)
  def plus(a:UShort, b:UShort): UShort = a + b
  override def pow(a:UShort, b:Int): UShort = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** UShort(b)
  }
  override def times(a:UShort, b:UShort): UShort = a * b
  def zero: UShort = UShort(0)
}

trait UIntIsRig extends Rig[UInt] {
  def one: UInt = UInt(1)
  def plus(a:UInt, b:UInt): UInt = a + b
  override def pow(a:UInt, b:Int): UInt = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** UInt(b)
  }
  override def times(a:UInt, b:UInt): UInt = a * b
  def zero: UInt = UInt(0)
}

trait ULongIsRig extends Rig[ULong] {
  def one: ULong = ULong(1)
  def plus(a:ULong, b:ULong): ULong = a + b
  override def pow(a:ULong, b:Int): ULong = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** ULong(b)
  }
  override def times(a:ULong, b:ULong): ULong = a * b
  def zero: ULong = ULong(0)
}

trait NaturalIsRig extends Rig[Natural] {
  def one: Natural = Natural(1L)
  def plus(a:Natural, b:Natural): Natural = a + b
  override def pow(a:Natural, b:Int): Natural = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a pow UInt(b)
  }
  override def times(a:Natural, b:Natural): Natural = a * b
  def zero: Natural = Natural(0L)
}
