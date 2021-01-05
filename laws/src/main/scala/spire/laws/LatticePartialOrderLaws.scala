package spire
package laws

import spire.algebra._
import spire.algebra.lattice._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import InvalidTestException._

object LatticePartialOrderLaws {
  def apply[A: Eq: Arbitrary] = new LatticePartialOrderLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LatticePartialOrderLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def joinSemilatticePartialOrder(implicit A: JoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "joinSemilatticePartialOrder",
      parents = Seq.empty,
      bases = Seq("order" -> OrderLaws[A].partialOrder, "lattice" -> LatticeLaws[A].joinSemilattice),
      "join.lteqv" -> forAllSafe((x: A, y: A) => (x <= y) === (y === (x.join(y))))
    )

  def meetSemilatticePartialOrder(implicit A: MeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "meetSemilatticePartialOrder",
      parents = Seq.empty,
      bases = Seq("order" -> OrderLaws[A].partialOrder, "lattice" -> LatticeLaws[A].meetSemilattice),
      "meet.lteqv" -> forAllSafe((x: A, y: A) => (x <= y) === (x === (x.meet(y))))
    )

  def latticePartialOrder(implicit A: Lattice[A], P: PartialOrder[A]) = new LatticePartialOrderProperties(
    name = "latticePartialOrder",
    parents = Seq(joinSemilatticePartialOrder, meetSemilatticePartialOrder),
    bases = Seq.empty
  )

  def boundedJoinSemilatticePartialOrder(implicit A: BoundedJoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedJoinSemilatticePartialOrder",
      parents = Seq(joinSemilatticePartialOrder),
      bases = Seq("lattice" -> LatticeLaws[A].boundedJoinSemilattice),
      "lteqv.zero" -> forAllSafe((x: A) => A.zero <= x)
    )

  def boundedMeetSemilatticePartialOrder(implicit A: BoundedMeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedMeetSemilatticePartialOrder",
      parents = Seq(meetSemilatticePartialOrder),
      bases = Seq("lattice" -> LatticeLaws[A].boundedMeetSemilattice),
      "lteqv.one" -> forAllSafe((x: A) => x <= A.one)
    )

  def boundedBelowLatticePartialOrder(implicit A: Lattice[A] with BoundedJoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedBelowLatticePartialOrder",
      parents = Seq(boundedJoinSemilatticePartialOrder, latticePartialOrder),
      bases = Seq("lattice" -> LatticeLaws[A].boundedBelowLattice)
    )

  def boundedAboveLatticePartialOrder(implicit A: Lattice[A] with BoundedMeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedAboveLatticePartialOrder",
      parents = Seq(boundedMeetSemilatticePartialOrder, latticePartialOrder),
      bases = Seq("lattice" -> LatticeLaws[A].boundedAboveLattice)
    )

  def boundedLatticePartialOrder(implicit A: BoundedLattice[A], P: PartialOrder[A]) = new LatticePartialOrderProperties(
    name = "boundedLatticePartialOrder",
    parents = Seq(boundedJoinSemilatticePartialOrder, boundedMeetSemilatticePartialOrder),
    bases = Seq("lattice" -> LatticeLaws[A].boundedLattice)
  )

  class LatticePartialOrderProperties(
    val name: String,
    val parents: Seq[LatticePartialOrderProperties],
    val bases: Seq[(String, Laws#RuleSet)],
    val props: (String, Prop)*
  ) extends RuleSet
}

// vim: expandtab:ts=2:sw=2
