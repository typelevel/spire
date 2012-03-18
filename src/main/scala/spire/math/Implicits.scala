package spire.math

import scala.{specialized => spec}

final class IntOpts(lhs:Int) {
}

final class FloatOps(lhs:Float) {
  @inline private final def c = Complex(lhs, 0.0F)
  def +(rhs:Complex[Float]) = c + rhs
  def *(rhs:Complex[Float]) = c * rhs
  def -(rhs:Complex[Float]) = c - rhs
  def /(rhs:Complex[Float]) = c / rhs
  def /~(rhs:Complex[Float]) = c /~ rhs
  def %(rhs:Complex[Float]) = c % rhs
  def /%(rhs:Complex[Float]) = c /% rhs
  def pow(rhs:Complex[Float]) = c pow rhs
  def **(rhs:Complex[Float]) = c ** rhs
  def ~^(rhs:Complex[Float]) = c ~^ rhs
}

final class DoubleOps(lhs:Double) {
  @inline private final def c = Complex(lhs, 0.0)
  def +(rhs:Complex[Double]) = c + rhs
  def *(rhs:Complex[Double]) = c * rhs
  def -(rhs:Complex[Double]) = c - rhs
  def /(rhs:Complex[Double]) = c / rhs
  def /~(rhs:Complex[Double]) = c /~ rhs
  def %(rhs:Complex[Double]) = c % rhs
  def /%(rhs:Complex[Double]) = c /% rhs
  def pow(rhs:Complex[Double]) = c pow rhs
  def **(rhs:Complex[Double]) = c ** rhs
  def ~^(rhs:Complex[Double]) = c ~^ rhs
}


object Implicits {
  implicit def eqOps[@spec(Int, Long, Float, Double) A:Eq](a:A) = new EqOps(a)
  implicit def orderOps[@spec(Int, Long, Float, Double) A:Order](a:A) = new OrderOps(a)
  implicit def semigroupOps[A:Semigroup](a:A) = new SemigroupOps(a)

  implicit def convertableOps[A:ConvertableFrom](a:A) = new ConvertableFromOps(a)

  implicit def ringOps[@spec(Int, Long, Float, Double) A:Ring](a:A) = new RingOps(a)
  implicit def euclideanRingOps[@spec(Int, Long, Float, Double) A:EuclideanRing](a:A) = new EuclideanRingOps(a)
  implicit def fieldOps[@spec(Float, Double) A:Field](a:A) = new FieldOps(a)

  implicit def fractionalOps[@spec(Float, Double) A:Fractional](a:A) = new FractionalOps(a)

  implicit def doubleOps(a:Double) = new DoubleOps(a)
  
  implicit def euclideanRingWithNRootOps[@spec(Int, Long) A: EuclideanRingWithNRoot](a: A) = new NRootOps(a)
}

