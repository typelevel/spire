package spire
package object scalacompat {

import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

  def arrayBuilderMake[T]()(implicit tag: ClassTag[T]): ArrayBuilder[T] =
      ArrayBuilder.make[T]

  def parallelSeq[A](s: Seq[A]): Seq[A] = {
    throw new Exception("parallel sequences are currently disabled for scala 2.13 and higher")
  }

  def preScala2p13: Boolean = false

  type SeqLike[A, C] = scala.collection.SeqOps[A, Seq, C]

  type IterableLike[A, C] = scala.collection.IterableOps[A, Iterable, C]
}
