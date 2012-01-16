package numerics.math

import scala.{specialized => spec}

class FloatOps(lhs:Float) {
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

class DoubleOps(lhs:Double) {
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

  implicit def ringOps[@spec(Int, Long, Float, Double) A:Ring](a:A) = new RingOps(a)
  implicit def euclideanRingOps[@spec(Int, Long, Float, Double) A:EuclideanRing](a:A) = new EuclideanRingOps(a)
  implicit def fieldOps[@spec(Float, Double) A:Field](a:A) = new FieldOps(a)

  implicit def fractionalOps[@spec(Float, Double) A:Fractional](a:A) = new FractionalOps(a)

  implicit def doubleOps(a:Double) = new DoubleOps(a)

  def eq[@spec(Int, Long, Float, Double) A:Eq] = implicitly[Eq[A]]
  def order[@spec(Int, Long, Float, Double) A:Order] = implicitly[Order[A]]
  def semigroup[A:Semigroup] = implicitly[Semigroup[A]]
  def monoid[A:Monoid] = implicitly[Monoid[A]]

  def ring[@spec(Int, Long, Float, Double) A:Ring] = implicitly[Ring[A]]
  def euclideanRing[@spec(Int, Long, Float, Double) A:EuclideanRing] = implicitly[EuclideanRing[A]]
  def field[@spec(Float, Double) A:Field] = implicitly[Field[A]]

  def orderedRing[@spec(Int, Long, Float, Double) A:OrderedRing] = implicitly[OrderedRing[A]]
  def integral[@spec(Int, Long) A:Integral]:Integral[A] = implicitly[Integral[A]]
  def fractional[@spec(Float, Double) A:Fractional]:Fractional[A] = implicitly[Fractional[A]]

  def numeric[@spec(Int, Long, Float, Double) A:Numeric] = implicitly[Numeric[A]]
}
