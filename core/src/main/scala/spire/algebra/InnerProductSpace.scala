package spire.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec

trait InnerProductSpace[V, @spec(Int, Long, Float, Double) F] extends VectorSpace[V, F] { self =>
  def dot(v: V, w: V): F

  def normed(implicit ev: NRoot[F]): NormedVectorSpace[V, F] =
    new NormedInnerProductSpace()(this, ev)
}

object InnerProductSpace {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: InnerProductSpace[V, R]) = V
}

@SerialVersionUID(1L)
private[algebra] class NormedInnerProductSpace[V, @spec(Float, Double) F]
    (implicit val vectorSpace: InnerProductSpace[V, F], val nroot: NRoot[F])
    extends NormedVectorSpace[V, F] with PassThroughVectorSpace[V, F] {
  def norm(v: V): F = nroot.sqrt(vectorSpace.dot(v, v))
}
