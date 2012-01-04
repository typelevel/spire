package numerics.math

import scala.{specialized => spec}

object Implicits {
  implicit def eqOps[@spec(Int, Long, Float, Double) A:Eq](a:A) = new EqOpsImpl(a)
  implicit def orderOps[@spec(Int, Long, Float, Double) A:Order](a:A) = new OrderOpsImpl(a)
  implicit def semigroupOps[A:Semigroup](a:A) = new SemigroupOpsImpl(a)

  implicit def ringOps[@spec(Int, Long, Float, Double) A:Ring](a:A) = new RingOpsImpl(a)
  implicit def euclideanRingOps[@spec(Int, Long, Float, Double) A:EuclideanRing](a:A) = new EuclideanRingOpsImpl(a)
  implicit def fieldOps[@spec(Float, Double) A:Field](a:A) = new FieldOpsImpl(a)

  def ring[@spec(Int, Long, Float, Double) A:Ring]:Ring[A] = implicitly[Ring[A]]
  def euclideanRing[@spec(Int, Long, Float, Double) A:EuclideanRing] = implicitly[EuclideanRing[A]]
  def field[@spec(Float, Double) A:Field]:Field[A] = implicitly[Field[A]]
  def integral[@spec(Int, Long) A:Integral]:Integral[A] = implicitly[Integral[A]]
  def fractional[@spec(Float, Double) A:Fractional]:Fractional[A] = implicitly[Fractional[A]]
  def numeric[@spec(Int, Long, Float, Double) A:Numeric]:Numeric[A] = implicitly[Numeric[A]]
}

final class EqOpsImpl[A](val lhs:A)(implicit val e:Eq[A]) extends EqOps[A]
final class OrderOpsImpl[A](val lhs:A)(implicit val o:Order[A]) extends OrderOps[A]
final class SemigroupOpsImpl[A](val lhs:A)(implicit val s:Semigroup[A]) extends SemigroupOps[A]
final class RingOpsImpl[A](val lhs:A)(implicit val n:Ring[A]) extends RingOps[A]
final class EuclideanRingOpsImpl[A](val lhs:A)(implicit val n:EuclideanRing[A]) extends EuclideanRingOps[A]
final class FieldOpsImpl[A](val lhs:A)(implicit val n:Field[A]) extends FieldOps[A]
