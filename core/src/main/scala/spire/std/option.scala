package spire.std

import spire.algebra._
import spire.algebra.{AdditiveSemigroup, AdditiveMonoid, MultiplicativeMonoid, MultiplicativeSemigroup}
import spire.syntax.all._

@SerialVersionUID(0L)
class OptionMonoid[A: Semigroup] extends Monoid[Option[A]] with Serializable {
  def id: Option[A] = None
  def isId(a: Option[A]) = a.isEmpty
  def op(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
    case (Some(x), Some(y)) => Some(x |+| y)
    case (None, None) => None
    case (x, None) => x
    case (None, y) => y
  }
}

@SerialVersionUID(0L)
class OptionCMonoid[A: CSemigroup] extends OptionMonoid[A] with CMonoid[Option[A]]

@SerialVersionUID(0L)
class OptionAdditiveMonoid[A: AdditiveSemigroup] extends AdditiveMonoid[Option[A]] with Serializable {
  def zero: Option[A] = None
  def plus(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
    case (Some(x), Some(y)) => Some(x + y)
    case (None, None) => None
    case (x, None) => x
    case (None, y) => y
  }
}

@SerialVersionUID(0L)
class OptionMultiplicativeMonoid[A: MultiplicativeSemigroup] extends MultiplicativeMonoid[Option[A]] with Serializable {
  def one: Option[A] = None
  def times(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
    case (Some(x), Some(y)) => Some(x * y)
    case (None, None) => None
    case (x, None) => x
    case (None, y) => y
  }
}

@SerialVersionUID(0L)
class OptionEq[A: Eq] extends Eq[Option[A]] with Serializable {
  def eqv(x: Option[A], y: Option[A]) = (x, y) match {
    case (Some(x), Some(y)) => Eq[A].eqv(x, y)
    case (None, None) => true
    case _ => false
  }
}

@SerialVersionUID(0L)
class OptionOrder[A: Order] extends OptionEq[A] with Order[Option[A]] with Serializable {
  override def eqv(x: Option[A], y: Option[A]) = (x, y) match {
    case (Some(x), Some(y)) => Eq[A].eqv(x, y)
    case (None, None) => true
    case _ => false
  }

  def compare(x: Option[A], y: Option[A]): Int = {
    (x, y) match {
      case (None, None) => 0
      case (None, Some(_)) => -1
      case (Some(_), None) => 1
      case (Some(x0), Some(y0)) => Order[A].compare(x0, y0)
    }
  }
}

trait OptionInstances0 {
  implicit def OptionEq[A: Eq] = new OptionEq[A]
  implicit def OptionMonoid[A: Semigroup] = new OptionMonoid[A]
}

trait OptionInstances extends OptionInstances0 {
  implicit def OptionCMonoid[A: CSemigroup] = new OptionCMonoid[A]
  implicit def OptionAdditiveMonoid[A: AdditiveSemigroup] = new OptionAdditiveMonoid[A]
  implicit def OptionMultiplicativeMonoid[A: MultiplicativeSemigroup] = new OptionMultiplicativeMonoid[A]

  implicit def OptionOrder[A: Order] = new OptionOrder[A]
}
