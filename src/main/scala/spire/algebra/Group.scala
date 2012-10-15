package spire.algebra

import spire.macros._
import language.experimental.macros

trait Group[A] extends Monoid[A] {
  def inverse(a: A): A
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]) = ev
}

class AdditiveGroup[A](implicit ring:Ring[A]) extends Group[A] {
  def id = ring.zero
  def inverse(a: A): A = ring.negate(a)
  def op(x:A, y:A) = ring.plus(x, y)
}

class MultiplicativeGroup[A](implicit field:Field[A]) extends Group[A] {
  def id = field.one
  def inverse(a: A): A = field.div(field.one, a)
  def op(x:A, y:A) = field.times(x, y)
}

final class GroupOps[A](lhs:A)(implicit ev:Group[A]) {
  def inverse() = macro Ops.unop[A]
}
