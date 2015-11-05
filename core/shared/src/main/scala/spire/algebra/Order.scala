package spire
package algebra

/**
  * The `Order` type class is used to define a total ordering on some type `A`.
  * An order is defined by a relation <=, which obeys the following laws:
  *
  * - either x <= y or y <= x (totality)
  * - if x <= y and y <= x, then x == y (antisymmetry)
  * - if x <= y and y <= z, then x <= z (transitivity)
  *
  * The truth table for compare is defined as follows:
  *
  * x <= y    x >= y      Int
  * true      true        = 0     (corresponds to x == y)
  * true      false       < 0     (corresponds to x < y)
  * false     true        > 0     (corresponds to x > y)
  *
  * By the totality law, x <= y and y <= x cannot be both false.
  */
trait Order[@sp A] extends Any with PartialOrder[A] {
  self =>

  def partialCompare(x: A, y: A): Double = compare(x, y).toDouble

  override def eqv(x: A, y: A): Boolean = compare(x, y) == 0
  override def gt(x: A, y: A): Boolean = compare(x, y) > 0
  override def lt(x: A, y: A): Boolean = compare(x, y) < 0
  override def gteqv(x: A, y: A): Boolean = compare(x, y) >= 0
  override def lteqv(x: A, y: A): Boolean = compare(x, y) <= 0

  def min(x: A, y: A): A = if (lt(x, y)) x else y
  def max(x: A, y: A): A = if (gt(x, y)) x else y
  def compare(x: A, y: A): Int

  /**
   * Defines an order on `B` by mapping `B` to `A` using `f` and using `A`s
   * order to order `B`.
   */
  override def on[@sp B](f: B => A): Order[B] = new MappedOrder(this)(f)

  /**
   * Defines an ordering on `A` where all arrows switch direction.
   */
  override def reverse: Order[A] = new ReversedOrder(this)
}

private[algebra] class MappedOrder[@sp A, @sp B](order: Order[B])(f: A => B) extends Order[A] {
  def compare(x: A, y: A): Int = order.compare(f(x), f(y))
}

private[algebra] class ReversedOrder[@sp A](order: Order[A]) extends Order[A] {
  def compare(x: A, y: A): Int = order.compare(y, x)
}

object Order {
  @inline final def apply[A](implicit o: Order[A]): Order[A] = o

  def by[@sp A, @sp B](f: A => B)(implicit o: Order[B]): Order[A] = o.on(f)

  def from[@sp A](f: (A, A) => Int): Order[A] = new Order[A] {
    def compare(x: A, y: A): Int = f(x, y)
  }

  implicit def ordering[A](implicit o: Order[A]): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A): Int = o.compare(x, y)
  }
}
