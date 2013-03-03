package spire.algebra

import spire.math._
import spire.macrosk.Ops
import java.lang.Math

import scala.{specialized => spec}

trait Field[@spec(Int,Long,Float,Double) A] extends EuclideanRing[A] with MultiplicativeAbGroup[A] {
  def ceil(a:A): A
  def floor(a:A): A
  def round(a:A): A
  def isWhole(a:A): Boolean
}

final class FieldOps[A](lhs:A)(implicit ev:Field[A]) {
  def isWhole() = macro Ops.unop[Boolean]
  def ceil() = macro Ops.unop[A]
  def floor() = macro Ops.unop[A]
  def round() = macro Ops.unop[A]
}

object Field {
  implicit object RealIsField extends RealIsField
  implicit def complexIsField[@spec(Float, Double) A: Fractional: Trig] =
    new ComplexIsField[A] {
      val f = Fractional[A]
      val t = Trig[A]
    }
  implicit object NumberIsField extends NumberIsField

  @inline final def apply[A](implicit f:Field[A]):Field[A] = f
}

trait RealIsField extends Field[Real] with RealIsEuclideanRing {
  def div(a:Real, b:Real) = a / b
  def ceil(a:Real) = if (a % 1 == 0) a else a + 1 - (a % 1)
  def floor(a:Real) = a - (a % 1)
  def round(a:Real) = {
    val m = a % 1
    if (m < 0.5) a - m else a + 1 - m
  }
  def isWhole(a:Real) = a % 1 == 0
}

trait ComplexIsField[@spec(Float,Double) A]
extends ComplexIsEuclideanRing[A] with Field[Complex[A]] {
  def div(a:Complex[A], b:Complex[A]) = a / b
  def ceil(a:Complex[A]): Complex[A] = a.ceil
  def floor(a:Complex[A]): Complex[A] = a.floor
  def round(a:Complex[A]): Complex[A] = a.round
  def isWhole(a:Complex[A]) = a.isWhole
}

trait NumberIsField extends Field[Number] with NumberIsEuclideanRing {
  def div(a:Number, b:Number) = a / b
  def ceil(a:Number): Number = a.ceil
  def floor(a:Number): Number = a.floor
  def round(a:Number): Number = a.round
  def isWhole(a:Number) = a.isWhole
}
