package spire.algebra
package free

final class FreeGroup[A] private (val terms: Vector[Either[A, A]]) extends AnyVal { lhs =>

  /**
   * Map each term to type `B` and sum them using `B`'s [[Group]].
   */
  def run[B](f: A => B)(implicit B: Group[B]): B =
    terms.foldLeft(B.id) {
      case (sum, Right(a)) => B.op(sum, f(a))
      case (sum, Left(a)) => B.opInverse(sum, f(a))
    }

  def |+|(rhs: FreeGroup[A]): FreeGroup[A] =
    reduce(lhs.terms.iterator ++ rhs.terms.iterator)

  def |-|(rhs: FreeGroup[A]): FreeGroup[A] =
    reduce(lhs.terms.iterator ++ rhs.terms.reverseIterator.map(_.swap))

  def inverse: FreeGroup[A] = {
    val bldr = Vector.newBuilder[Either[A, A]]
    terms.reverseIterator foreach { term =>
      bldr += term.swap
    }
    new FreeGroup(bldr.result())
  }

  private def reduce(it: Iterator[Either[A, A]]): FreeGroup[A] = {
    def annihilated(x: Either[A, A], y: Either[A, A]): Boolean = (x, y) match {
      case (Left(x0), Right(y0)) if x0 == y0 => true
      case (Right(x0), Left(y0)) if x0 == y0 => true
      case _ => false
    }

    def loop(acc: Vector[Either[A, A]]): Vector[Either[A, A]] =
      if (it.hasNext) {
        val cand = it.next()
        if (acc.nonEmpty && annihilated(acc.last, cand)) {
          loop(acc.init)
        } else {
          loop(acc :+ cand)
        }
      } else acc

    new FreeGroup(loop(Vector.empty))
  }

  override def toString: String =
    if (terms.isEmpty) "e"
    else {
      val init = terms.head match {
        case Left(h) => s"($h).inverse"
        case Right(h) => h.toString
      }
      val tail = terms.tail.map {
        case Left(x) => s" |-| $x"
        case Right(x) => s" |+| $x"
      }
      init + tail.mkString
    }
}

object FreeGroup { companion =>
  final def id[A]: FreeGroup[A] = new FreeGroup(Vector.empty)

  final def apply[A](a: A): FreeGroup[A] = lift(a)
  final def lift[A](a: A): FreeGroup[A] = new FreeGroup[A](Vector(Right(a)))

  implicit def FreeGroupGroup[A]: Group[FreeGroup[A]] = new Group[FreeGroup[A]] {
    def id: FreeGroup[A] = companion.id
    def op(a: FreeGroup[A], b: FreeGroup[A]): FreeGroup[A] = a |+| b
    def inverse(a: FreeGroup[A]): FreeGroup[A] = a.inverse
    override def opInverse(a: FreeGroup[A], b: FreeGroup[A]): FreeGroup[A] = a |-| b
  }
}
