package spire
package laws

import spire.algebra._
import spire.algebra.lattice._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import InvalidTestException._

object LatticeLaws {
  def apply[A : Eq : Arbitrary] = new LatticeLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LatticeLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]


  def joinSemilattice(implicit A: JoinSemilattice[A]) = new LatticeProperties(
    name = "joinSemilattice",
    parents = Nil,
    "join.associative" -> forAllSafe((x: A, y: A, z: A) =>
      ((x join y) join z) === (x join (y join z))
    ),
    "join.commutative" -> forAllSafe((x: A, y: A) =>
      (x join y) === (y join x)
    ),
    "join.idempotent" -> forAllSafe((x: A) =>
      (x join x) === x
    )
  )

  def meetSemilattice(implicit A: MeetSemilattice[A]) = new LatticeProperties(
    name = "meetSemilattice",
    parents = Nil,
    "meet.associative" -> forAllSafe((x: A, y: A, z: A) =>
      ((x meet y) meet z) === (x meet (y meet z))
    ),
    "meet.commutative" -> forAllSafe((x: A, y: A) =>
      (x meet y) === (y meet x)
    ),
    "meet.idempotent" -> forAllSafe((x: A) =>
      (x meet x) === x
    )
  )

  def lattice(implicit A: Lattice[A]) = new LatticeProperties(
    name = "lattice",
    parents = Seq(joinSemilattice, meetSemilattice),
    "absorption" -> forAllSafe((x: A, y: A) =>
      ((x join (x meet y)) === x) &&
        ((x meet (x join y)) === x)
    )
  )

  def boundedJoinSemilattice(implicit A: BoundedJoinSemilattice[A]) = new LatticeProperties(
    name = "boundedJoinSemilattice",
    parents = Seq(joinSemilattice),
    "join.identity" -> forAllSafe((x: A) =>
      (x join A.zero) === x && (A.zero join x) === x
    )
  )

  def boundedMeetSemilattice(implicit A: BoundedMeetSemilattice[A]) = new LatticeProperties(
    name = "boundedMeetSemilattice",
    parents = Seq(meetSemilattice),
      "meet.identity" -> forAllSafe((x: A) =>
        (x meet A.one) === x && (A.one meet x) === x
      )
  )

  def boundedBelowLattice(implicit A: Lattice[A] with BoundedJoinSemilattice[A]) = new LatticeProperties(
    name = "boundedBelowLattice",
    parents = Seq(boundedJoinSemilattice, lattice)
  )

  def boundedAboveLattice(implicit A: Lattice[A] with BoundedMeetSemilattice[A]) = new LatticeProperties(
    name = "boundedAboveLattice",
    parents = Seq(boundedMeetSemilattice, lattice)
  )

  def boundedLattice(implicit A: BoundedLattice[A]) = new LatticeProperties(
    name = "boundedLattice",
    parents = Seq(boundedJoinSemilattice, boundedMeetSemilattice, lattice)
  )

  class LatticeProperties(
    val name: String,
    val parents: Seq[LatticeProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq.empty
  }

}

// vim: expandtab:ts=2:sw=2
