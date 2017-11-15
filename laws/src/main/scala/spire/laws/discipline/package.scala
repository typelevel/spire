package spire.laws

import spire.algebra.Eq
import spire.laws.shadows.{Shadow, Shadowing}

import org.scalacheck.util.Pretty
import org.scalacheck.{Arbitrary, Prop}

package object discipline {

  def eqvToProp[A:Eq](x: A, y: A)(implicit pp: A => Pretty): Prop =
    if (Eq[A].eqv(x, y)) Prop.passed else Prop.falsified :| {
      val xp = Pretty.pretty[A](x, Pretty.Params(0))
      val yp = Pretty.pretty[A](y, Pretty.Params(0))
      xp + " is not === to " + yp
    }

  implicit def spireLawsIsEqToProp[A:Eq](isEq: IsEq[A]): Prop =
    isEq.fold((x, y) => eqvToProp(x, y), Prop.passed) // TODO: replace passed by undecided?

  implicit def spireLawsArbitraryShadow[A, S](implicit A: Arbitrary[A], S: Shadowing[A, S]): Arbitrary[Shadow[A, S]] =
    Arbitrary { A.arbitrary.map( a => Shadow(a, S.toShadow(a)) ) }
}
