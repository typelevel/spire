package spire.algebra

import scala.{specialized => spec}

/**
  * The `PartialOrder` type class is used to define a partial ordering on some type `A`.
  * 
  * A partial order is defined by a relation <=, which obeys the following laws:
  * 
  * - x <= x (reflexivity)
  * - if x <= y and y <= x, then x === y (anti-symmetry)
  * - if x <= y and y <= z, then x <= z (transitivity)
  * 
  * To compute both <= and >= at the same time, we use a Double number
  * to encode the result of the comparisons x <= y and x >= y.
  * The truth table is defined as follows:
  * 
  * x <= y    x >= y      Double
  * true      true        = 0.0     (corresponds to x === y)
  * false     false       = NaN     (x and y cannot be compared)
  * true      false       = -1.0    (corresponds to x < y)
  * false     true        = 1.0     (corresponds to x > y)
  * 
  */
trait PartialOrder[@spec A] extends Eq[A] {
  self =>
  /** Result of comparing `x` with `y`. Returns NaN if operands
    * are not comparable. If operands are comparable, returns a
    * Double whose sign is:
    * - negative iff `x < y`
    * - zero     iff `x === y`
    * - positive iff `x > y`
    */
  def partialCompare(x: A, y: A): Double
  /** Result of comparing `x` with `y`. Returns None if operands
    * are not comparable. If operands are comparable, returns Some[Int]
    * where the Int sign is:
    * - negative iff `x < y`
    * - zero     iff `x == y`
    * - positive iff `x > y`
    */  
  def tryCompare(x: A, y: A): Option[Int] = {
    val c = partialCompare(x, y)
    if (c.isNaN) None else Some(c.signum)
  }

  /** Returns Some(x) if x <= y, Some(y) if x > y, otherwise None. */
  def pmin(x: A, y: A): Option[A] = {
    val c = partialCompare(x, y)
    if (c <= 0) Some(x)
    else if (c > 0) Some(y)
    else None
  }

  /** Returns Some(x) if x >= y, Some(y) if x < y, otherwise None. */
  def pmax(x: A, y: A): Option[A] = {
    val c = partialCompare(x, y)
    if (c >= 0)  Some(x)
    else if (c < 0) Some(y)
    else None
  }

  // The following should be overriden in priority for performance
  def eqv(x: A, y: A): Boolean = partialCompare(x, y) == 0
  def lteqv(x: A, y: A): Boolean = partialCompare(x, y) <= 0
  def lt(x: A, y: A): Boolean = partialCompare(x, y) < 0

  def gteqv(x: A, y: A): Boolean = lteqv(y, x)
  def gt(x: A, y: A): Boolean = lt(y, x)

  /**
   * Defines a partial order on `B` by mapping `B` to `A` using `f` and using `A`s
   * order to order `B`.
   */
  override def on[@spec B](f: B => A): PartialOrder[B] = new MappedPartialOrder(this)(f)

  /**
   * Defines a partial order on `A` where all arrows switch direction.
   */
  def reverse: PartialOrder[A] = new ReversedPartialOrder(this)
}

private[algebra] class MappedPartialOrder[@spec A, @spec B](partialOrder: PartialOrder[B])(f: A => B) extends PartialOrder[A] {
  def partialCompare(x: A, y: A): Double = partialOrder.partialCompare(f(x), f(y))
}

private[algebra] class ReversedPartialOrder[@spec A](partialOrder: PartialOrder[A]) extends PartialOrder[A] {
  def partialCompare(x: A, y: A): Double = partialOrder.partialCompare(y, x)
}

object PartialOrder {
  @inline final def apply[A](implicit po: PartialOrder[A]) = po

  def by[@spec A, @spec B](f: A => B)(implicit po: PartialOrder[B]): PartialOrder[A] = po.on(f)

  def from[@spec A](f: (A, A) => Double): PartialOrder[A] = new PartialOrder[A] {
    def partialCompare(x: A, y: A) = f(x, y)
  }

  implicit def partialOrdering[A](implicit po: PartialOrder[A]) = new PartialOrdering[A] {
    def tryCompare(x: A, y: A): Option[Int] = po.tryCompare(x, y)
    def lteq(x: A, y: A): Boolean = po.lteqv(x, y)
  }
}

private[algebra] class DerivedPartialOrdering[@spec A](partialOrder: PartialOrder[A]) extends PartialOrdering[A] {
  def tryCompare(x: A, y: A): Option[Int] = partialOrder.tryCompare(x, y)
  def lteq(x: A, y: A): Boolean = partialOrder.lteqv(x, y)
}
