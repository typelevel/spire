package spire
package object scalacompat {

import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

  def arrayBuilderMake[T]()(implicit tag: ClassTag[T]): ArrayBuilder[T] =
      ArrayBuilder.make[T]

  def parallelSeq[A](s: Seq[A]): Seq[A] = {
    println("parallel sequences are currently disabled for scala 2.13 and higher")
    s
  }

  type SeqLike[A, C] = scala.collection.SeqOps[A, Seq, C]

  type IterableLike[A, C] = scala.collection.IterableOps[A, Iterable, C]
}
