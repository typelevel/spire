package spire.algebra

import spire.std._

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.annotation.tailrec


trait EuclideanCoordinateSpace[V, @spec(Float, Double) F] extends Any with CoordinateSpace[V, F] {
  override def dimensions : Int = 3
  def cross(v: V, w: V): V 
  def angle(v: V, w: V)(implicit ev: NRoot[F], trig: Trig[F]): F
}

object EuclideanCoordinateSpace {
  @inline final def apply[V, @spec(Float,Double) F](implicit V: EuclideanCoordinateSpace[V, F]) = V

  def seq[A: Field, CC[A] <: SeqLike[A, CC[A]]](implicit cbf0: CanBuildFrom[CC[A], A, CC[A]]) = 
     new EuclideanSeqCoordinateSpace[A, CC[A]]

  def array[@spec(Float, Double) A: Field: ClassTag]: EuclideanCoordinateSpace[Array[A], A] =
    new EuclideanArrayCoordinateSpace[A]
}




