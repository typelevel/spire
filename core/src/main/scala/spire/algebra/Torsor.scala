package spire.algebra

import scala.{specialized => sp}

trait Torsor[V, @sp(Int,Long,Float,Double) R] extends GroupAction[V, R] { self =>
  implicit def scalar: AbGroup[R]

  def diff(v: V, w: V): R

  def fixOrigin(id0: V): AbGroup[V] =
    new AbGroup[V] {
      def id: V = id0
      def op(v: V, w: V): V = self.actl(self.diff(v, id0), w)
      def inverse(v: V): V = self.actl(self.diff(id0, v), id0)
      override def opInverse(v: V, w: V): V = self.actl(self.diff(v, w), id0)
    }
}

trait AdditiveTorsor[V, @sp(Int,Long,Float,Double) R] extends AdditiveGroupAction[V, R] { self =>
  implicit def scalar: AdditiveAbGroup[R]

  def pminus(v: V, w: V): R

  def fixOrigin(id: V): AdditiveAbGroup[V] =
    new AdditiveAbGroup[V] {
      def zero: V = id
      def plus(v: V, w: V): V = self.gplusl(self.pminus(v, id), w)
      def negate(v: V): V = self.gplusl(self.pminus(id, v), id)
      override def minus(v: V, w: V): V = self.gplusl(self.pminus(v, w), id)
    }
}

trait MultiplicativeTorsor[V, @sp(Int,Long,Float,Double) R]
    extends MultiplicativeGroupAction[V, R] { self =>
  implicit def scalar: MultiplicativeAbGroup[R]

  def pdiv(v: V, w: V): R

  def fixOrigin(id: V): MultiplicativeAbGroup[V] =
    new MultiplicativeAbGroup[V] {
      def one: V = id
      def times(v: V, w: V): V = self.gtimesl(self.pdiv(v, id), w)
      override def reciprocal(v: V): V = self.gtimesl(self.pdiv(id, v), id)
      def div(v: V, w: V): V = self.gtimesl(self.pdiv(v, w), id)
    }
}

object Torsor {
  @inline final def apply[V, @sp(Int,Long,Float,Double) R](implicit V: Torsor[V, R]) = V
}
