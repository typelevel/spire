package spire

import spire.algebra._
import spire.implicits._

import org.scalacheck.{Prop, Properties}

import org.typelevel.discipline.Predicate

package object laws {

  implicit final class IsEqArrow[A](lhs: => A) {
    def <=>(rhs: => A): IsEq[A] = try {
      IsEq.valid(lhs, rhs)
    } catch {
      case e: IsEq.InvalidTestException => IsEq.invalid[A]
    }
  }

  implicit def PredicateFromMonoid[A: Eq](implicit A: AdditiveMonoid[A]): Predicate[A] = new Predicate[A] {
    def apply(a: A) = a =!= A.zero
  }

  def propertiesToProp(properties: Properties) = Prop.all(properties.properties.map(_._2): _*)

}

// vim: expandtab:ts=2:sw=2
