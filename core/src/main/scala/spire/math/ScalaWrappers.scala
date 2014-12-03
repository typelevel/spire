package spire.math

import spire.algebra.{Eq, EuclideanRing, Field, PartialOrder, Order, Ring, Signed}

private[spire] trait ScalaEquivWrapper[A] extends scala.math.Equiv[A] {
  def eq: Eq[A]

  def equiv(x:A, y:A): Boolean = eq.eqv(x, y)
}

private[spire] trait ScalaPartialOrderingWrapper[A] extends scala.math.PartialOrdering[A] {
  def partialOrder: PartialOrder[A]

  def tryCompare(x:A, y:A): Option[Int] = partialOrder.tryCompare(x, y)

  override def equiv(x:A, y:A): Boolean = partialOrder.eqv(x, y)
  override def gt(x:A, y:A): Boolean = partialOrder.gt(x, y)
  override def gteq(x:A, y:A): Boolean = partialOrder.gteqv(x, y)
  override def lt(x:A, y:A): Boolean = partialOrder.lt(x, y)
  override def lteq(x:A, y:A): Boolean = partialOrder.lteqv(x, y)
}


private[spire] trait ScalaOrderingWrapper[A] extends scala.math.Ordering[A] {
  def order: Order[A]

  def compare(x:A, y:A): Int = order.compare(x, y)

  override def equiv(x:A, y:A): Boolean = order.eqv(x, y)
  override def gt(x:A, y:A): Boolean = order.gt(x, y)
  override def gteq(x:A, y:A): Boolean = order.gteqv(x, y)
  override def lt(x:A, y:A): Boolean = order.lt(x, y)
  override def lteq(x:A, y:A): Boolean = order.lteqv(x, y)

  override def min(x:A, y:A): A = order.min(x, y)
  override def max(x:A, y:A): A = order.max(x, y)
}

private[spire] trait ScalaNumericWrapper[A] extends scala.math.Numeric[A] with ScalaOrderingWrapper[A] {
  def structure: Ring[A]
  def conversions: ConvertableFrom[A]
  def signed: Signed[A]
  def order: Order[A]

  def fromInt(x: Int) = structure.fromInt(x)
  def negate(x:A) = structure.negate(x)
  def minus(x:A, y:A) = structure.minus(x, y)
  def plus(x:A, y:A) = structure.plus(x, y)
  def times(x:A, y:A) = structure.times(x, y)
  override def zero: A = structure.zero
  override def one: A = structure.one

  def toDouble(x: A) = conversions.toDouble(x)
  def toFloat(x: A) = conversions.toFloat(x)
  def toInt(x: A) = conversions.toInt(x)
  def toLong(x: A) = conversions.toLong(x)

  override def signum(x:A): Int = signed.signum(x)
  override def abs(x: A): A = signed.abs(x)
}

private[spire] trait ScalaFractionalWrapper[A] extends ScalaNumericWrapper[A] with scala.math.Fractional[A] {
  def structure: Field[A]

  def div(x:A, y:A) = structure.div(x, y)
}

private[spire] trait ScalaIntegralWrapper[A] extends ScalaNumericWrapper[A] with scala.math.Integral[A]{
  def structure: EuclideanRing[A]

  def quot(x:A, y:A): A = structure.quot(x, y)
  def rem(x:A, y:A): A = structure.mod(x, y)
}
