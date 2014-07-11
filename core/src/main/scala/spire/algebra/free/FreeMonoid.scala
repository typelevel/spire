package spire.algebra
package free

final class FreeMonoid[A] private (val terms: List[A]) extends AnyVal { lhs =>

  /**
   * Map each term to type `B` and sum them using `B`'s [[Semigroup]],
   * as long as there is at least 1 term. Otherwise, return `None`.
   */
  def runOption[B](f: A => B)(implicit B: Semigroup[B]): Option[B] =
    terms match {
      case head :: tail =>
        Some(tail.foldLeft(f(head)) { (acc, a) => B.op(acc, f(a)) })
      case Nil =>
        None
    }

  /**
   * Map each term to type `B` and sum them using `B`'s [[Monoid]].
   */
  def run[B](f: A => B)(implicit B: Monoid[B]): B =
    terms.foldLeft(B.id) { (acc, a) => B.op(acc, f(a)) }

  def |+|(rhs: FreeMonoid[A]): FreeMonoid[A] =
    new FreeMonoid(lhs.terms ++ rhs.terms)

  override def toString: String =
    terms.mkString(" |+| ")
}

object FreeMonoid { companion =>
  final def id[A]: FreeMonoid[A] = new FreeMonoid(Nil)

  final def apply[A](a: A): FreeMonoid[A] = lift(a)
  final def lift[A](a: A): FreeMonoid[A] = new FreeMonoid[A](a :: Nil)

  implicit def FreeMonoidMonoid[A]: Monoid[FreeMonoid[A]] = new Monoid[FreeMonoid[A]] {
    def id: FreeMonoid[A] = companion.id
    def op(a: FreeMonoid[A], b: FreeMonoid[A]): FreeMonoid[A] = a |+| b
  }
}
