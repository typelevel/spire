package spire
package object scalacompat {

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ArrayBuilder, Builder}
import scala.collection.parallel.ParSeq
import scala.reflect.ClassTag

  def arrayBuilderMake[T]()(implicit tag: ClassTag[T]): ArrayBuilder[T] =
      ArrayBuilder.make[T]()

  def parallelSeq[A](s: Seq[A]): ParSeq[A] = s.par

  def preScala2p13: Boolean = true

  type SeqLike[A, C] = scala.collection.SeqLike[A, C]

  type IterableLike[A, C] = scala.collection.IterableLike[A, C]

  private[spire] type Factory[-A, +C] = CanBuildFrom[Nothing, A, C]

  private[spire] implicit class FactoryCompatOps[-A, +C](private val factory: Factory[A, C]) {
    def fromSpecific(it: TraversableOnce[A]): C = (factory() ++= it).result()

    def newBuilder: Builder[A, C] = factory()
  }
}
