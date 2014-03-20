package spire.algebra

import scala.{specialized => sp}

trait Torsor[V, @sp(Int,Long,Float,Double) R] { self =>
  implicit def scalar: AbGroup[R]

  def opl(r: R, v: V): V
  def opr(v: V, r: R): V = opl(r, v)

  def opInverse(v: V, w: V): R

  def fixOrigin(id0: V): AbGroup[V] =
    new AbGroup[V] {
      def id: V = id0
      def op(v: V, w: V): V = self.opl(self.opInverse(v, id0), w)
      def inverse(v: V): V = self.opl(self.opInverse(id0, v), id0)
      override def opInverse(v: V, w: V): V = self.opl(self.opInverse(v, w), id0)
    }
}

trait AdditiveTorsor[V, @sp(Int,Long,Float,Double) R] { self =>
  implicit def scalar: AdditiveAbGroup[R]

  def plusl(r: R, v: V): V
  def plusr(v: V, r: R): V = plusl(r, v)

  def minus(v: V, w: V): R

  def fixOrigin(id: V): AdditiveAbGroup[V] =
    new AdditiveAbGroup[V] {
      def zero: V = id
      def plus(v: V, w: V): V = self.plusl(self.minus(v, id), w)
      def negate(v: V): V = self.plusl(self.minus(id, v), id)
      override def minus(v: V, w: V): V = self.plusl(self.minus(v, w), id)
    }
}

trait MultiplicativeTorsor[V, @sp(Int,Long,Float,Double) R] { self =>
  implicit def scalar: MultiplicativeAbGroup[R]

  def timesl(r: R, v: V): V
  def timesr(v: V, r: R): V = timesl(r, v)

  def div(v: V, w: V): R

  def fixOrigin(id: V): MultiplicativeAbGroup[V] =
    new MultiplicativeAbGroup[V] {
      def one: V = id
      def times(v: V, w: V): V = self.timesl(self.div(v, id), w)
      override def reciprocal(v: V): V = self.timesl(self.div(id, v), id)
      def div(v: V, w: V): V = self.timesl(self.div(v, w), id)
    }
}

object Torsor {
  @inline final def apply[V, @sp(Int,Long,Float,Double) R](implicit V: Torsor[V, R]) = V
}
