package spire.algebra
package std

trait OptionMonoid[A] extends Monoid[Option[A]] {
  def scalar: Semigroup[A]

  def id: Option[A] = None
  def op(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
    case (Some(x), Some(y)) => Some(scalar.op(x, y))
    case (None, None) => None
    case (x, None) => x
    case (None, y) => y
  }
}

trait OptionEq[A] extends Eq[Option[A]] {
  def A: Eq[A]
  def eqv(x: Option[A], y: Option[A]) = (x, y) match {
    case (Some(x), Some(y)) => A.eqv(x, y)
    case (None, None) => true
    case _ => false
  }
}

trait OptionOrder[A] extends OptionEq[A] with Order[Option[A]] {
  def A: Order[A]
  def compare(x: Option[A], y: Option[A]): Int = {
    (x, y) match {
      case (None, None) => 0
      case (None, Some(_)) => -1
      case (Some(_), None) => 1
      case (Some(x0), Some(y0)) => A.compare(x0, y0)
    }
  }
}

trait OptionInstances0 {
  implicit def OptionEq[A: Eq] = new OptionEq[A] {
    val A = Eq[A]
  }
}

trait OptionInstances extends OptionInstances0 {
  implicit def OptionMonoid[A: Semigroup] = new OptionMonoid[A] {
    val scalar = Semigroup[A]
  }

  implicit def OptionOrder[A: Order] = new OptionOrder[A] {
    val A = Order[A]
  }
}
