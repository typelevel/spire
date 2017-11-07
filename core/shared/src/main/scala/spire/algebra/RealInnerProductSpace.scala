package spire
package algebra

// what to do with this? https://math.stackexchange.com/questions/49348/inner-product-spaces-over-finite-fields
// we should allow the use of complex vector spaces

trait RealInnerProductSpace[V, @sp(Int, Long, Float, Double) F] extends Any with VectorSpace[V, F] { self =>
  def dot(v: V, w: V): F

  def normed(implicit ev: NRoot[F]): RealNormedVectorSpace[V, F] = new NormedInnerProductSpace[V, F] {
    def space = self
    def nroot: NRoot[F] = ev
  }
}

object RealInnerProductSpace {
  @inline final def apply[V, @sp(Int,Long,Float,Double) R](implicit V: RealInnerProductSpace[V, R]): RealInnerProductSpace[V, R] = V
}

private[algebra] trait NormedInnerProductSpace[V, @sp(Float, Double) F] extends Any with RealNormedVectorSpace[V, F] {
  def space: RealInnerProductSpace[V, F]
  def scalar: Field[F] = space.scalar
  def nroot: NRoot[F]

  def zero: V = space.zero
  def plus(v: V, w: V): V = space.plus(v, w)
  def negate(v: V): V = space.negate(v)
  override def minus(v: V, w: V): V = space.minus(v, w)
  def timesl(f: F, v: V): V = space.timesl(f, v)
  override def divr(v: V, f: F): V = space.divr(v, f)
  def norm(v: V): F = nroot.sqrt(space.dot(v, v))
}
