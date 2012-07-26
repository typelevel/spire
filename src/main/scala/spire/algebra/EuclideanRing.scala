package spire.algebra

import spire.math._
import spire.macros._

import language.experimental.macros
import scala.{specialized => spec}
import scala.math.{abs, ceil, floor}

trait EuclideanRing[@spec(Int,Long,Float,Double) A] extends Ring[A] {
  def quot(a:A, b:A):A
  def mod(a:A, b:A):A
  def quotmod(a:A, b:A) = (quot(a, b), mod(a, b))
}

final class EuclideanRingOps[A](lhs:A)(implicit ev:EuclideanRing[A]) {
  def /~(rhs:A) = macro Ops.binop[A, A]
  def %(rhs:A) = macro Ops.binop[A, A]
  def /%(rhs:A) = macro Ops.binop[A, A]
}

object EuclideanRing {
  implicit object IntIsEuclideanRing extends IntIsEuclideanRing
  implicit object LongIsEuclideanRing extends LongIsEuclideanRing
  implicit object FloatIsEuclideanRing extends FloatIsEuclideanRing
  implicit object DoubleIsEuclideanRing extends DoubleIsEuclideanRing
  implicit object BigIntIsEuclideanRing extends BigIntIsEuclideanRing
  implicit object BigDecimalIsEuclideanRing extends BigDecimalIsEuclideanRing
  implicit object RationalIsEuclideanRing extends RationalIsEuclideanRing
  implicit object RealIsEuclideanRing extends RealIsEuclideanRing
  implicit def complexIsEuclideanRingCls[A:Fractional:Trig] = new ComplexIsEuclideanRingCls

  def apply[A](implicit e:EuclideanRing[A]):EuclideanRing[A] = e
}


trait IntIsEuclideanRing extends EuclideanRing[Int] with IntIsRing {
  def quot(a:Int, b:Int) = a / b
  def mod(a:Int, b:Int) = a % b
}

trait LongIsEuclideanRing extends EuclideanRing[Long] with LongIsRing {
  def quot(a:Long, b:Long) = a / b
  def mod(a:Long, b:Long) = a % b
}

trait FloatIsEuclideanRing extends EuclideanRing[Float] with FloatIsRing {
  def quot(a:Float, b:Float) = {
    val d = a / b
    if (d < 0.0) ceil(d).toFloat else floor(d).toFloat
  }
  def mod(a:Float, b:Float) = a % b
}

trait DoubleIsEuclideanRing extends EuclideanRing[Double] with DoubleIsRing {
  def quot(a:Double, b:Double) = {
    val d = a / b
    if (d < 0.0) ceil(d) else floor(d)
  }
  def mod(a:Double, b:Double) = a % b
}

trait BigIntIsEuclideanRing extends EuclideanRing[BigInt] with BigIntIsRing {
  def quot(a:BigInt, b:BigInt) = a / b
  def mod(a:BigInt, b:BigInt) = a % b
}

trait BigDecimalIsEuclideanRing extends EuclideanRing[BigDecimal] with BigDecimalIsRing {
  def quot(a:BigDecimal, b:BigDecimal) = a.quot(b)
  def mod(a:BigDecimal, b:BigDecimal) = a % b
}

trait RationalIsEuclideanRing extends EuclideanRing[Rational] with RationalIsRing {
  def quot(a:Rational, b:Rational) = a.quot(b)
  def mod(a:Rational, b:Rational) = a % b
}

trait RealIsEuclideanRing extends EuclideanRing[Real] with RealIsRing {
  def quot(a: Real, b: Real): Real = a /~ b
  def mod(a: Real, b: Real): Real = a % b
}

trait ComplexIsEuclideanRing[@spec(Float,Double) A]
extends ComplexIsRing[A] with EuclideanRing[Complex[A]] {
  implicit val f:Fractional[A]

  override def quotmod(a:Complex[A], b:Complex[A]) = a /% b
  def quot(a:Complex[A], b:Complex[A]) = a /~ b
  def mod(a:Complex[A], b:Complex[A]) = a % b
}

class ComplexIsEuclideanRingCls[@spec(Float,Double) A]
(implicit val f:Fractional[A], val t:Trig[A]) extends ComplexIsEuclideanRing[A]
