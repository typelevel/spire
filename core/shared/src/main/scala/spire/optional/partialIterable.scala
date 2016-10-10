package spire
package optional

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

import spire.algebra.{Semigroup, Group}
import spire.algebra.partial.{Semigroupoid, Groupoid}
import spire.util._

final class IterableSemigroupoid[A, SA <: IterableLike[A, SA]](implicit cbf: CanBuildFrom[SA, A, SA], A: Semigroup[A]) extends Semigroupoid[SA] {
  override def opIsDefined(x: SA, y: SA): Boolean = x.size == y.size
  def partialOp(x: SA, y: SA): Opt[SA] =
    if (opIsDefined(x, y)) Opt({
      val xIt = x.iterator
      val yIt = y.iterator
      val builder = cbf()
      while (xIt.nonEmpty) {
        assert(yIt.nonEmpty)
        builder += A.combine(xIt.next, yIt.next)
      }
      builder.result()
    }) else Opt.empty[SA]
}

final class IterableGroupoid[A, SA <: IterableLike[A, SA]](implicit cbf: CanBuildFrom[SA, A, SA], A: Group[A]) extends Groupoid[SA] {
  override def opIsDefined(x: SA, y: SA): Boolean = x.size == y.size
  def partialOp(x: SA, y: SA): Opt[SA] =
    if (opIsDefined(x, y)) Opt({
      val xIt = x.iterator
      val yIt = y.iterator
      val builder = cbf()
      while (xIt.nonEmpty) {
        assert(yIt.nonEmpty)
        builder += A.combine(xIt.next, yIt.next)
      }
      builder.result()
    }) else Opt.empty[SA]
  def inverse(a: SA): SA = a.map(A.inverse(_))(cbf)
  override def leftId(a: SA): SA = a.map(x => A.empty)(cbf)
  override def rightId(a: SA): SA = a.map(x => A.empty)(cbf)
}

trait PartialIterable0 {
  implicit def IterableSemigroupoid[A: Semigroup, CC[A] <: IterableLike[A, CC[A]]](implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): Semigroupoid[CC[A]] = new IterableSemigroupoid[A, CC[A]]
}

trait PartialIterable1 extends PartialIterable0 {
  implicit def IterableGroupoid[A: Group, CC[A] <: IterableLike[A, CC[A]]](implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): Groupoid[CC[A]] = new IterableGroupoid[A, CC[A]]
}

object partialIterable extends PartialIterable1
