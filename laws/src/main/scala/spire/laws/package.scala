package spire

import spire.algebra._
import spire.implicits._

import org.scalacheck.{Prop, Properties}
import org.scalacheck.util.Pretty

import org.typelevel.discipline.Predicate

package object laws {

  /** Provides special syntax to use in laws catching invalid tests.
    * 
    * Invalid tests are those where the range of a primitive type is
    * exceeded: in that case, the behavior is undecided with respect
    * to the laws.
    */
  implicit final class EqArrow[A](lhs: => A) {
    def <=>(rhs: => A)(implicit equ: Eq[A], pp: A => Pretty): Prop =
      try {
        if (Eq[A].eqv(lhs, rhs)) Prop.passed else Prop.falsified :| {
          val lp = Pretty.pretty[A](lhs, Pretty.Params(0))
          val rp = Pretty.pretty[A](rhs, Pretty.Params(0))
          s"$lp is not === to $rp"
        }
      } catch {
        case e: shadows.InvalidTestException => Prop.passed // TODO: or undecided?
      }
  }

  def checkTrue(b: => Prop): Prop = {
    try {
      b
    } catch {
      case e: shadows.InvalidTestException => Prop.passed // TODO: or undecided?
    }
  }

  implicit def PredicateFromMonoid[A: Eq](implicit A: AdditiveMonoid[A]): Predicate[A] = new Predicate[A] {
    def apply(a: A) = a =!= A.zero
  }

  def propertiesToProp(properties: Properties) = Prop.all(properties.properties.map(_._2): _*)

}

// vim: expandtab:ts=2:sw=2
