package spire.example

import scala.collection.{IterableOps, IterableFactory, IterableFactoryDefaults}

trait BigStreamCompat[A] extends IterableOps[A, BigStream, BigStream[A]]
  with IterableFactoryDefaults[A, BigStream] { this: BigStream[A] =>
  override def iterableFactory: IterableFactory[BigStream] = BigStream
}

trait BigStreamCompanionCompat extends IterableFactory[BigStream] {
  def from[A](source: IterableOnce[A]): BigStream[A] =
    source.iterator.foldLeft(BigStream.empty[A])((t, a) => new BigCons(a, t))
}
