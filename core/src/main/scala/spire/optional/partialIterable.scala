package spire
package optional

import spire.scalacompat.{Factory, FactoryCompatOps, IterableLike}

import spire.algebra.{Semigroup, Group}
import spire.algebra.partial.{Semigroupoid, Groupoid}
import spire.util._

final class IterableSemigroupoid[A, SA <: IterableLike[A, SA]](implicit cbf: Factory[A, SA], A: Semigroup[A]) extends Semigroupoid[SA] {
  override def opIsDefined(x: SA, y: SA): Boolean = x.size == y.size
  def partialOp(x: SA, y: SA): Opt[SA] =
    if (opIsDefined(x, y)) Opt({
      val xIt = x.iterator
      val yIt = y.iterator
      val builder = cbf.newBuilder
      while (xIt.nonEmpty) {
        assert(yIt.nonEmpty)
        builder += A.combine(xIt.next(), yIt.next())
      }
      builder.result()
    }) else Opt.empty[SA]
}

final class IterableGroupoid[A, SA <: IterableLike[A, SA]](implicit cbf: Factory[A, SA], A: Group[A]) extends Groupoid[SA] {
  override def opIsDefined(x: SA, y: SA): Boolean = x.size == y.size
  def partialOp(x: SA, y: SA): Opt[SA] =
    if (opIsDefined(x, y)) Opt({
      val xIt = x.iterator
      val yIt = y.iterator
      val builder = cbf.newBuilder
      while (xIt.nonEmpty) {
        assert(yIt.nonEmpty)
        builder += A.combine(xIt.next(), yIt.next())
      }
      builder.result()
    }) else Opt.empty[SA]
  def inverse(a: SA): SA = cbf.newBuilder.++=(a.map(A.inverse(_))).result()
  override def leftId(a: SA): SA = cbf.newBuilder.++=(a.map(x => A.empty)).result()
  override def rightId(a: SA): SA = cbf.newBuilder.++=(a.map(x => A.empty)).result()
}

trait PartialIterable0 {
  implicit def IterableSemigroupoid[A: Semigroup, CC[A] <: IterableLike[A, CC[A]]](implicit cbf: Factory[A, CC[A]]): Semigroupoid[CC[A]] = new IterableSemigroupoid[A, CC[A]]
}

trait PartialIterable1 extends PartialIterable0 {
  implicit def IterableGroupoid[A: Group, CC[A] <: IterableLike[A, CC[A]]](implicit cbf: Factory[A, CC[A]]): Groupoid[CC[A]] = new IterableGroupoid[A, CC[A]]
}

object partialIterable extends PartialIterable1
