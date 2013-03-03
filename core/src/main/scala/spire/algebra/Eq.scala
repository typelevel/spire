package spire.algebra

import scala.{specialized => spec}

import spire.math._
import spire.macrosk.Ops

trait Eq[@spec A] {
  def eqv(x:A, y:A): Boolean
  def neqv(x:A, y:A): Boolean = !eqv(x, y)
  def on[@spec B](f:B => A): Eq[B] = new MappedEq(this)(f)
}

class MappedEq[@spec A, @spec B](eq: Eq[B])(f: A => B) extends Eq[A] {
  def eqv(x: A, y: A): Boolean = eq.eqv(f(x), f(x))
}

final class EqOps[A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A) = macro Ops.binop[A, Boolean]
  def =!=(rhs:A) = macro Ops.binop[A, Boolean]
}

object Eq {
  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@spec A, @spec B](f:A => B)(implicit e:Eq[B]): Eq[A] = new MappedEq(e)(f)
}
