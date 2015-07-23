package spire

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.Predicate

package object laws {

  implicit def PredicateFromMonoid[A: Eq](implicit A: AdditiveMonoid[A]): Predicate[A] = new Predicate[A] {
    def apply(a: A) = a =!= A.zero
  }

}

// vim: expandtab:ts=2:sw=2
