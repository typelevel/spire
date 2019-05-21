package spire.example

import scala.collection.IterableLike
import scala.collection.mutable.Builder

trait BigStreamCompat[A] extends IterableLike[A, BigStream[A]] {
  override def newBuilder: Builder[A, BigStream[A]] = BigStream.newBuilder[A]
}

trait BigStreamCompanionCompat
