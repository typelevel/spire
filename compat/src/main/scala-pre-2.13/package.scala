package spire
package object scalacompat {

import scala.collection.mutable.ArrayBuilder
import scala.collection.parallel.ParSeq
import scala.reflect.ClassTag

  def arrayBuilderMake[T]()(implicit tag: ClassTag[T]): ArrayBuilder[T] =
      ArrayBuilder.make[T]()

  def parallelSeq[A](s: Seq[A]): ParSeq[A] = s.par

  def preScala2p13: Boolean = true

  type SeqLike[A, C] = scala.collection.SeqLike[A, C]

  type IterableLike[A, C] = scala.collection.IterableLike[A, C]
}
