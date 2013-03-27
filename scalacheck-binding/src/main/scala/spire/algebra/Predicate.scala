package spire.algebra

import spire.implicits._

trait Predicate[A] extends (A => Boolean) {
  def apply(a: A): Boolean
  def &&(that: Predicate[A]) = Predicate[A](a => this(a) && that(a))
}

object Predicate {

  def apply[A](f: A => Boolean) = new Predicate[A] {
    def apply(a: A) = f(a)
  }

  def const[A](res: Boolean) = new Predicate[A] {
    def apply(a: A) = res
  }

  implicit def PredicateFromMonoid[A: Eq](implicit A: AdditiveMonoid[A]): Predicate[A] = new Predicate[A] {
    def apply(a: A) = a =!= A.zero
  }

}

// vim: expandtab:ts=2:sw=2
